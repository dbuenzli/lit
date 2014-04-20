(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Tgl3
open Lit
open Renderer.Private 

let str = Format.asprintf

(* Renderer ids. *) 

module Id = struct
  type t = int 
  let compare : int -> int -> int = Pervasives.compare
end

module Imap = Map.Make (Id) 
module Smap = Map.Make (String)

module Gl_hi = struct                         (* Wraps a few Gl functions. *) 

  let get_string len =
    let a = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
    fun f -> f a; Gl.string_of_bigarray a

  let get_int =
    let a = Ba.create Ba.Int32 1 in
    fun f -> f a; Int32.to_int a.{0}
        
  let set_int = 
    let a = Ba.create Ba.Int32 1 in 
    fun f id -> a.{0} <- Int32.of_int id; f a
        
  (* Buffers objects *) 

  let gen_buffer () = get_int (Gl.gen_buffers 1) 
  let delete_buffer id = set_int (Gl.delete_buffers 1) id

  (* Vertex array objects *) 

  let gen_vertex_array () = get_int (Gl.gen_vertex_arrays 1) 
  let delete_vertex_array id = set_int (Gl.delete_vertex_arrays 1) id

  (* Texture objects *) 

  let gen_texture () = get_int (Gl.gen_textures 1) 
  let delete_texture id = set_int (Gl.delete_textures 1) id

  (* Render buffer objects *) 

  let gen_render_buffer () = get_int (Gl.gen_renderbuffers 1)
  let delete_render_buffer id = set_int (Gl.delete_renderbuffers 1) id

  (* Framebuffer objects *) 
      
  let gen_framebuffer () = get_int (Gl.gen_framebuffers 1)
  let delete_framebuffer id = set_int (Gl.delete_framebuffers 1) id
      
  (* Shader objects *) 

  let get_shader_int sid e = get_int (Gl.get_shaderiv sid e)
  let get_shader_info_log sid =
    let len = get_shader_int sid Gl.info_log_length in
    get_string len (Gl.get_shader_info_log sid len None)

  (* Program objects *) 

  let get_program_int pid e = get_int (Gl.get_programiv pid e)
  let get_program_info_log pid = 
    let len = get_program_int pid Gl.info_log_length in 
    get_string len (Gl.get_program_info_log pid len None)

  (* Errors *) 

  let error_to_string = function 
  | e when e = Gl.no_error -> "no error" 
  | e when e = Gl.invalid_enum -> "invalid enum" 
  | e when e = Gl.invalid_value -> "invalid value" 
  | e when e = Gl.invalid_operation -> "invalid operation" 
  | e when e = Gl.invalid_framebuffer_operation -> 
      "invalid framebuffer operation"
  | e when e = Gl.out_of_memory -> 
      "out of memory" 
  | e -> (Printf.sprintf "unknown 0x%04X" e)

  let check_error () = 
    let stack = Printexc.get_callstack 2 in
    let e = Gl.get_error () in 
    if e = Gl.no_error then () else 
    begin
     let stack = Printexc.raw_backtrace_to_string stack in
      let loc = 
        try
          let nl = String.index stack '\n' in
          String.sub stack nl (String.length stack - nl - 1)
        with Not_found -> "????:??" 
      in
      Printf.eprintf "%s: %s\n%!" loc (error_to_string e)
    end

  (* Enum maps *) 
    
  module Enum = struct
    type t = Gl.enum 
    let compare : int -> int -> int = Pervasives.compare
  end
  
  module Emap = Map.Make (Enum)
  
  let enum_map : not_found:(Gl.enum -> 'a) -> (Gl.enum * 'a) list ->  
    (Gl.enum -> 'a) = fun ~not_found l ->
    let add acc (e, v) = Emap.add e v acc in
    let m = List.fold_left add Emap.empty l in 
    fun e -> try Emap.find e m with Not_found -> not_found e
end

(* Renderer *) 

type t = 
  { mutable caps : Cap.t;
    mutable debug : bool;
    mutable log : Log.t; 
    mutable compiler_msg_parser : Log.compiler_msg_parser;
    mutable cleanups : (unit -> unit) list;
    mutable raster : Effect.raster; (* Last setup raster state *) 
    mutable depth : Effect.depth;   (* Last setup depth state *) 
    mutable blend : Effect.blend;   (* Last setup blend state *)
    mutable fbuf : fbuf;
    mutable size : size2;
    mutable view : view;
    mutable world_to_clip : m4 Lazy.t; (* cache *) 
    mutable 
      batches : op list Imap.t; (* maps programs ids to rendering operations *) 
    }

type renderer = t

let log_error r msg = r.log `Error msg 
let log_debug r msg = r.log `Debug msg

(* Backend info 

   Generic backend info handler. Automatically adds cleanup 
   actions to the renderer whenever the backend info is finalized. 
   We can't execute OpenGL calls directly in the finalizer as
   those functions may be executed by an arbitrary thread which 
   leads to segfaults in most GL implementation. *) 

module type BVal = sig
  type t
  val finalizer_add_action : renderer -> t -> (unit -> unit)
end

module type BInfoable = sig
  type t
  val binfo : t -> BInfo.t
  val set_binfo : t -> BInfo.t -> unit
end

module BInfo_handler (M : BInfoable) (BVal : BVal) : sig 
  val binfo : M.t -> BVal.t option
  val get_binfo : M.t -> BVal.t
  val set_binfo : renderer -> M.t -> BVal.t -> unit
end = struct
  let inject, project = BInfo.create () 
  let binfo b = project (M.binfo b)
  let get_binfo b = match binfo b with None -> assert false | Some i -> i
  let set_binfo r b (i : BVal.t) = 
    let finalize i = 
      r.cleanups <- BVal.finalizer_add_action r i :: r.cleanups 
    in
    Gc.finalise finalize i;
    M.set_binfo b (inject i)  
end

(* Backend caps *) 

module BCap = struct
             
  let get_version enum = match Gl.get_string enum with 
  | None -> `Unknown 
  | Some v -> 
      match Cap.parse_version v with 
      | Some (x, y, z) -> `GL (x, y, z)
      | None -> `Unknown 
        
  let gl_version () = get_version Gl.version 
  let glsl_version () = get_version Gl.shading_language_version 
  
  let gl_renderer () = match Gl.get_string Gl.renderer with 
  | None -> "unknown" 
  | Some r -> r

  let gl_vendor () = match Gl.get_string Gl.vendor with 
  | None -> "unknown" 
  | Some v -> v    

  let shader_stages v = 
    let base = [ `Vertex; `Fragment; `Geometry ] in 
    match v with
    | `GL (maj, min, _) when maj >= 4 ->
        let base = `Tess_control :: `Tess_evaluation :: base in 
        if min >= 3 then `Compute :: base else base 
    | `GL _ -> base 
    | `Unknown -> base
    | _ -> assert false 
      
  let get () = 
    let gl_version = gl_version () in
    let open Cap in
    { c_shader_stages = shader_stages gl_version; 
      c_max_samples = Gl_hi.get_int (Gl.get_integerv Gl.max_samples); 
      c_max_tex_size = Gl_hi.get_int (Gl.get_integerv Gl.max_texture_size); 
      c_max_render_buffer_size = 
        Gl_hi.get_int (Gl.get_integerv Gl.max_renderbuffer_size);
      c_gl_version = gl_version; 
      c_glsl_version = glsl_version (); 
      c_gl_renderer = gl_renderer (); 
      c_gl_vendor = gl_vendor (); }

  let caps r = r.caps
end

(* Backend buffers  *) 

module BBuf = struct

  let enum_of_scalar_type = function 
  | `Int8 -> Gl.byte
  | `Int16 -> Gl.short
  | `Int32 -> Gl.int
  | `Int64 -> failwith "TODO"
  | `UInt8 -> Gl.unsigned_byte
  | `UInt16 -> Gl.unsigned_short
  | `UInt32 -> Gl.unsigned_int
  | `UInt64 -> failwith "TODO"
  | `Float16 -> Gl.half_float 
  | `Float32 -> Gl.float
  | `Float64 -> Gl.double

  let enum_of_usage = function 
  | `Static_draw -> Gl.static_draw
  | `Static_read -> Gl.static_read
  | `Static_copy -> Gl.static_copy
  | `Stream_draw -> Gl.stream_draw
  | `Stream_read -> Gl.stream_read
  | `Stream_copy -> Gl.stream_copy
  | `Dynamic_draw -> Gl.dynamic_draw
  | `Dynamic_read -> Gl.dynamic_read
  | `Dynamic_copy -> Gl.dynamic_copy

  let enum_of_access = function
  | `R -> Gl.read_only 
  | `W -> Gl.write_only 
  | `RW -> Gl.read_write 
   
  type binfo = 
    { id : Id.t;          (* Associated GL buffer object id. *) 
      scalar_type : Gl.enum;           (* Buffer scalar type *) }

  module BVal = struct
    type t = binfo 
    let finalizer_add_action _ i = fun () ->
      if i.id <> 0 then Gl_hi.delete_buffer i.id
  end

  include BInfo_handler (Buf) (BVal)

  let setup r b =
    let id = match binfo b with 
    | Some binfo -> binfo.id
    | None -> 
        let id = Gl_hi.gen_buffer () in
        let i = { id; scalar_type = enum_of_scalar_type (Buf.scalar_type b) } in
        set_binfo r b i; 
        i.id
    in
    if not (Buf.gpu_upload b) then id else (* TODO unclear for `Gpu *) 
    let usage = enum_of_usage (Buf.usage b) in
    Gl.bind_buffer Gl.array_buffer id; 
    Gl.buffer_data Gl.array_buffer 0 None usage; (* mark buffer as unused. *) 
    begin match Buf.cpu_p b with 
    | None -> 
        let byte_count = Buf.gpu_byte_count b in
        Gl.buffer_data Gl.array_buffer byte_count None usage
    | Some (Buf.Ba ba) -> 
        let byte_count = Buf.cpu_byte_count b in
        Gl.buffer_data Gl.array_buffer byte_count (Some ba) usage;
        Buf.set_gpu_count b (Buf.cpu_count b);
    end;
    Gl.bind_buffer Gl.array_buffer 0; 
    Buf.set_gpu_exists b true;
    Buf.set_gpu_upload b false;
    if Buf.cpu_autorelease b then Buf.set_cpu b None; 
    id

  let sync_cpu_to_gpu r b = 
    if not (Buf.cpu_exists b) then invalid_arg "no cpu buffer" else
    Buf.set_gpu_upload b true; 
    ignore (setup r b)

  let sync_gpu_to_cpu r b = 
    if not (Buf.gpu_exists b) then invalid_arg "no gpu buffer" else 
    let id = setup r b in
    let gpu_kount = Buf.gpu_count b in
    if Buf.cpu_count b <> gpu_kount 
    then Buf.(set_cpu_p b (create_bigarray_any (scalar_type b) gpu_kount));
    match Buf.cpu_p b with 
    | None -> assert false 
    | Some (Buf.Ba ba) ->
        let byte_count = Buf.cpu_byte_count b in
        Gl.bind_buffer Gl.array_buffer id;
        Gl.get_buffer_sub_data Gl.array_buffer 0 byte_count ba;
        Gl.bind_buffer Gl.array_buffer 0
      
  (* Mapping the GPU buffer. 
     
     FIXME: Improve safety. What could be done is keep a ref 
     on the returned big array in binfo, and set its length to 
     zero once we unmap (would need to do it in C I guess).  *)

  let gpu_map r access b st = 
    Buf.check_ba_scalar_type b st;
    let id = setup r b in
    let kind = Ba.ba_kind_of_ba_scalar_type st in
    let access = enum_of_access access in
    let count = Buf.gpu_count b in
    Gl.bind_buffer Gl.array_buffer id; 
    let ba = Gl.map_buffer Gl.array_buffer count access kind in 
    Gl.bind_buffer Gl.array_buffer 0;
    ba

  let gpu_unmap r b = 
    let id = setup r b in 
    Gl.bind_buffer Gl.array_buffer id;
    ignore (Gl.unmap_buffer Gl.array_buffer); 
    Gl.bind_buffer Gl.array_buffer 0
end

(* Backend prims *) 

module BPrim = struct
            
  let enum_of_kind = function
  | `Points -> Gl.points
  | `Lines -> Gl.lines
  | `Triangles -> Gl.triangles
  | `Line_strip -> Gl.line_strip
  | `Line_loop -> Gl.line_loop
  | `Lines_adjacency -> Gl.lines_adjacency
  | `Line_strip_adjacency -> Gl.line_strip_adjacency
  | `Triangle_fan -> Gl.triangle_fan
  | `Triangle_strip -> Gl.triangle_strip
  | `Triangles_adjacency -> Gl.triangles_adjacency
  | `Triangle_strip_adjacency -> Gl.triangles_adjacency
                              
  type binfo =                     
    { id : Id.t;                 (* Associated GL vertex array object. *) 
      kind : Gl.enum;                  (* kind of primitive (GL mode). *) 
      mutable locs : int Smap.t; (* maps attr names to bound location. *) 
      mutable last_prog : Id.t; }  (* last program id it was bound to. 
                                      FIXME. Potentially there could be a 
                                      race with that optim if a GL 
                                      implementation quickly recycles IDs. *)

  module BVal = struct
    type t = binfo 
    let finalizer_add_action _ i = fun () ->
      if i.id <> 0 then Gl_hi.delete_vertex_array i.id
  end

  include BInfo_handler (Prim) (BVal)
  
  let setup r p = match binfo p with 
  | Some _ -> 
      let update_attr a = ignore (BBuf.setup r (Attr.buf a)) in
      Prim.iter update_attr p; 
      begin match Prim.index p with 
      | None -> () 
      | Some b -> ignore (BBuf.setup r b)
      end
  | None -> 
      let id = Gl_hi.gen_vertex_array () in
      let kind = enum_of_kind (Prim.kind p) in
      let i = { id; kind; locs = Smap.empty; last_prog = 0 } in 
      let index_id = match Prim.index p with 
      | None -> 0 
      | Some index -> BBuf.setup r index
      in
      set_binfo r p i;
      Prim.iter (fun a -> ignore (BBuf.setup r (Attr.buf a))) p;
      Gl.bind_vertex_array i.id;
      Gl.bind_buffer Gl.element_array_buffer index_id;
      (* attrs are bound later, see Prog.bind_prim *) 
      Gl.bind_vertex_array 0; 
      Gl.bind_buffer Gl.element_array_buffer 0
end

(* Backend textures *) 

module BTex = struct
  let target_enum_of_kind = function 
  | `D1 -> Gl.texture_1d 
  | `D2 -> Gl.texture_2d 
  | `D3 -> Gl.texture_3d 
  | `D2_ms -> Gl.texture_2d_multisample
  | `D3_ms -> Gl.texture_2d_multisample_array
  | `Buffer -> Gl.texture_buffer

  let enum_of_min_filter = function 
  | `Linear -> Gl.linear
  | `Linear_mipmap_linear -> Gl.linear_mipmap_linear
  | `Linear_mipmap_nearest -> Gl.linear_mipmap_nearest
  | `Nearest -> Gl.nearest
  | `Nearest_mipmap_linear -> Gl.nearest_mipmap_linear
  | `Nearest_mipmap_nearest -> Gl.nearest_mipmap_nearest

  let enum_of_mag_filter = function 
  | `Linear -> Gl.linear
  | `Nearest -> Gl.nearest

  let enum_of_wrap = function
  | `Clamp_to_edge -> Gl.clamp_to_edge
  | `Mirrored_repeat -> Gl.mirrored_repeat
  | `Repeat -> Gl.repeat

  let internal_format_enum_of_format : Lit.Tex.sample_format -> Gl.enum = 
    function 
    | `D4 fmt -> 
        begin match fmt with 
        | `Float32, _ -> Gl.rgba32f
        | `Int8, norm -> if norm then Gl.rgba8_snorm else Gl.rgba8i
        | `Int16, norm -> if norm then Gl.rgba16_snorm else Gl.rgba16i
        | `UInt8, norm ->  if norm then Gl.rgba8 else Gl.rgba8ui
        | `UInt16, norm -> if norm then Gl.rgba16 else Gl.rgba16ui
        | _ -> failwith "TODO"
        end                                
    | `D3 fmt -> 
        begin match fmt with 
        | `Float32, _ -> Gl.rgb32f
        | `Int8, norm -> if norm then Gl.rgb8_snorm else Gl.rgb8i
        | `Int16, norm -> if norm then Gl.rgb16_snorm else Gl.rgb16i
        | `UInt8, norm -> if norm then Gl.rgb8 else Gl.rgb8ui
        | `UInt16, norm -> if norm then Gl.rgb16 else Gl.rgb16ui
        | _ -> failwith "TODO"
        end
    | `D2 fmt -> 
        begin match fmt with 
        | `Float32, _ -> Gl.rg32f
        | `Int8, norm -> if norm then Gl.rg8_snorm else Gl.rg8i
        | `Int16, norm -> if norm then Gl.rg16_snorm else Gl.rg16i
        | `UInt8, norm -> if norm then Gl.rg8 else Gl.rg8ui
        | `UInt16, norm -> if norm then Gl.rg16 else Gl.rg16ui
        | _ -> failwith "TODO"
        end
    | `D1 fmt -> 
        begin match fmt with
        | `Float32, _ -> Gl.r32f
        | `Int8, norm -> if norm then Gl.r8_snorm else Gl.r8i
        | `Int16, norm -> if norm then Gl.r16_snorm else Gl.r16i
        | `UInt8, norm -> if norm then Gl.r8 else Gl.r8ui
        | `UInt16, norm -> if norm then Gl.r16 else Gl.r16ui
        | _ -> failwith "TODO"
        end
    | `SRGB `UInt8 -> Gl.srgb8
    | `SRGBA `UInt8 -> Gl.srgb8_alpha8
    | `Depth fmt -> 
        begin match fmt with 
        | `UInt16 -> Gl.depth_component16
        | `UInt24 -> Gl.depth_component24
        | `Float32 -> Gl.depth_component32f
        end
    | `Depth_stencil fmt -> 
        begin match fmt with 
        | `UInt24_UInt8 -> Gl.depth24_stencil8 
        | `Float32_UInt8 -> Gl.depth32f_stencil8
        end
    | `Stencil `UInt8 -> Gl.stencil_index8

  type int_scalar_type = [ `Int8 | `Int16 | `Int32 | `Int64 
                         | `UInt8 | `UInt16 | `UInt32 | `UInt64 ]

  let format_enum_of_format : Lit.Tex.sample_format -> Gl.enum = function 
  | `D1 (#int_scalar_type, false) -> Gl.red_integer 
  | `D1 (_, _) -> Gl.red 
  | `D2 (#int_scalar_type, false) -> Gl.rg_integer
  | `D2 (_, _) -> Gl.rg
  | `D3 (#int_scalar_type, false) -> Gl.rgb_integer
  | `D3 (_, _) | `SRGB _ -> Gl.rgb
  | `D4 (#int_scalar_type, false) -> Gl.rgba_integer
  | `D4 (_, _) | `SRGBA _ -> Gl.rgba
  | `Depth _ -> Gl.depth_component 
  | `Depth_stencil _ -> Gl.depth_stencil
  | `Stencil _ -> Gl.stencil_index

  let type_enum_of_format : Tex.sample_format -> Gl.enum = function 
  | `D1 (`Int8, _) | `D2 (`Int8, _) | `D3 (`Int8, _) | `D4 (`Int8, _) ->
      Gl.byte
  | `D1 (`UInt8, _) | `D2 (`UInt8, _) | `D3 (`UInt8, _) | `D4 (`UInt8, _)
  | `SRGB (`UInt8) | `SRGBA (`UInt8) | `Stencil (`UInt8) -> 
      Gl.unsigned_byte          
  | `D1 (`Int16, _) | `D2 (`Int16, _) | `D3 (`Int16, _) | `D4 (`Int16, _) ->
      Gl.short
  | `D1 (`UInt16, _) | `D2 (`UInt16, _) | `D3 (`UInt16, _) | `D4 (`UInt16, _)
  | `Depth (`UInt16) -> 
      Gl.unsigned_short
  | `D1 (`Float32, _) | `D2 (`Float32, _) | `D3 (`Float32, _) 
  | `D4 (`Float32, _) | `Depth (`Float32) ->
    Gl.float
  | _ -> (* TODO *) failwith "TODO"

  
  type binfo = { id : Id.t;  }
               
  module BVal = struct
    type t = binfo
    let finalizer_add_action _ i = fun () -> 
      if i.id <> 0 then Gl_hi.delete_texture i.id      
  end

  include BInfo_handler (Tex) (BVal)

  let setup r t = 
    if t == Tex.nil then 0 else
    let id = match binfo t with
    | Some binfo -> binfo.id
    | None -> 
        let id = Gl_hi.gen_texture () in 
        let i = { id } in
        set_binfo r t i; 
        i.id
    in
    if not (Tex.gpu_update t) then id else
    let buf_id = match Tex.buf t with 
    | None -> 0 
    | Some b -> BBuf.setup r b
    in
    let kind = Tex.kind t in
    let target = target_enum_of_kind kind in
    let tex_format = Tex.sample_format t in
    let internal = internal_format_enum_of_format tex_format in 
    let format = format_enum_of_format tex_format in 
    let type_ = type_enum_of_format tex_format in
    let size = Tex.size3 t in
    let wrap_s = enum_of_wrap (Tex.wrap_s t) in 
    let wrap_t = enum_of_wrap (Tex.wrap_t t) in 
    let wrap_r = enum_of_wrap (Tex.wrap_r t) in 
    let w = Float.int_of_round (Size3.w size) in 
    let h = Float.int_of_round (Size3.h size) in 
    let d = Float.int_of_round (Size3.d size) in
    Gl.pixel_storei Gl.unpack_alignment 1;
    if buf_id <> 0 then Gl.bind_buffer Gl.pixel_unpack_buffer buf_id;
    Gl.bind_texture target id;
    begin match kind with
    | `D1 -> 
        Gl.tex_parameteri target Gl.texture_wrap_s wrap_s; 
        Gl.tex_image1d target 0 internal w 0 format type_ (`Offset 0);
    | `D2 -> 
        Gl.tex_parameteri target Gl.texture_wrap_s wrap_s; 
        Gl.tex_parameteri target Gl.texture_wrap_t wrap_t; 
        Gl.tex_image2d target 0 internal w h 0 format type_ (`Offset 0);
    | `D3 -> 
        Gl.tex_parameteri target Gl.texture_wrap_s wrap_s; 
        Gl.tex_parameteri target Gl.texture_wrap_t wrap_t; 
        Gl.tex_parameteri target Gl.texture_wrap_r wrap_r; 
        Gl.tex_image3d target 0 internal w h d 0 format type_ (`Offset 0);
    | `D2_ms -> 
        let scount, fixed = Tex.multisample t in
        Gl.tex_image2d_multisample target scount internal w h fixed;
    | `D3_ms -> 
        let scount, fixed = Tex.multisample t in
        Gl.tex_image3d_multisample target scount internal w h d fixed
    | `Buffer -> 
        Gl.tex_buffer target internal buf_id
    end;
    if kind = `D1 || kind = `D2 || kind = `D3 then begin
      let mag = enum_of_mag_filter (Tex.mag_filter t) in 
      let min = enum_of_min_filter (Tex.min_filter t) in
      Gl.tex_parameteri target Gl.texture_mag_filter mag; 
      Gl.tex_parameteri target Gl.texture_min_filter min;
      if Tex.mipmaps t then Gl.generate_mipmap target;
    end;
    Gl.bind_texture target 0;
    Gl.bind_buffer Gl.pixel_unpack_buffer 0; 
    Tex.set_gpu_update t false;
    if Tex.buf_autorelease t then Tex.set_buf t None;
    id
end

(* Backend programs *) 

module BProg = struct

  type attr_type = 
    [ `Float32 of [ `V1 | `V2 | `V3 | `V4 | `M2 | `M3 | `M4 | `M23 | `M24 
                  | `M32 | `M32 | `M32 | `M34 | `M42 | `M43 ]
    | `Int32 of [ `V1 | `V2 | `V3 | `V4 ]
    | `UInt32 of [ `V1 | `V2 | `V3 | `V4 ]
    | `Unsupported of int ]

  type uniform_type =
    [ `Int32 of [ `V1 | `V2 | `V3 | `V4 ]
    | `UInt32 of [ `V1 | `V2 | `V3 | `V4 ]
    | `Float32 of [ `V1 | `V2 | `V3 | `V4 | `M2 | `M3 | `M4 ]
    | `Bool of [ `V1 | `V2 | `V3 | `V4 ] 
    | `Sampler
    | `Unsupported of int ]

  let uniform_type_of_enum = 
    Gl_hi.enum_map ~not_found:(fun e -> `Unsupported e) [
      Gl.float, `Float32 `V1;
      Gl.float_vec2, `Float32 `V2; 
      Gl.float_vec3, `Float32 `V3; 
      Gl.float_vec4, `Float32 `V4;
      Gl.float_mat2, `Float32 `M2;
      Gl.float_mat3, `Float32 `M3;
      Gl.float_mat4, `Float32 `M4;
      Gl.int, `Int32 `V1;
      Gl.int_vec2, `Int32 `V2; 
      Gl.int_vec3, `Int32 `V3; 
      Gl.int_vec4, `Int32 `V4;
      Gl.unsigned_int, `UInt32 `V1;
      Gl.unsigned_int_vec2, `UInt32 `V2; 
      Gl.unsigned_int_vec3, `UInt32 `V3; 
      Gl.unsigned_int_vec4, `UInt32 `V4;
      Gl.bool, `Bool `V1;
      Gl.bool_vec2, `Bool `V2; 
      Gl.bool_vec3, `Bool `V3; 
      Gl.bool_vec4, `Bool `V4;
      Gl.sampler_1d, `Sampler; 
      Gl.sampler_2d, `Sampler; 
      Gl.sampler_3d, `Sampler; 
      Gl.sampler_cube, `Sampler; 
      Gl.sampler_1d_shadow, `Sampler; 
      Gl.sampler_2d_shadow, `Sampler; 
      Gl.sampler_1d_array, `Sampler; 
      Gl.sampler_2d_array, `Sampler; 
      Gl.sampler_1d_array_shadow, `Sampler; 
      Gl.sampler_2d_array_shadow, `Sampler; 
      Gl.sampler_2d_multisample, `Sampler; 
      Gl.sampler_2d_multisample_array, `Sampler; 
      Gl.sampler_cube_shadow, `Sampler; 
      Gl.sampler_buffer, `Sampler; 
      Gl.sampler_2d_rect, `Sampler;
      Gl.sampler_2d_rect_shadow, `Sampler; 
      Gl.sampler_1d_shadow, `Sampler; 
      Gl.sampler_2d_shadow, `Sampler; 
      Gl.sampler_1d_array, `Sampler; 
      Gl.sampler_2d_array, `Sampler; 
      Gl.sampler_1d_array_shadow, `Sampler; 
      Gl.sampler_2d_array_shadow, `Sampler; 
      Gl.sampler_2d_multisample, `Sampler; 
      Gl.sampler_2d_multisample_array, `Sampler; 
      Gl.sampler_cube_shadow, `Sampler; 
      Gl.sampler_buffer, `Sampler; 
      Gl.sampler_2d_rect, `Sampler;
      Gl.sampler_2d_rect_shadow, `Sampler; 
      Gl.int_sampler_1d, `Sampler; 
      Gl.int_sampler_2d, `Sampler; 
      Gl.int_sampler_3d, `Sampler; 
      Gl.int_sampler_cube, `Sampler; 
      Gl.int_sampler_1d_array, `Sampler; 
      Gl.int_sampler_2d_array, `Sampler; 
      Gl.int_sampler_2d_multisample, `Sampler; 
      Gl.int_sampler_2d_multisample_array, `Sampler; 
      Gl.int_sampler_buffer, `Sampler; 
      Gl.int_sampler_2d_rect, `Sampler;
      Gl.unsigned_int_sampler_1d, `Sampler; 
      Gl.unsigned_int_sampler_2d, `Sampler; 
      Gl.unsigned_int_sampler_3d, `Sampler; 
      Gl.unsigned_int_sampler_cube, `Sampler; 
      Gl.unsigned_int_sampler_1d_array, `Sampler; 
      Gl.unsigned_int_sampler_2d_array, `Sampler; 
      Gl.unsigned_int_sampler_2d_multisample, `Sampler; 
      Gl.unsigned_int_sampler_2d_multisample_array, `Sampler; 
      Gl.unsigned_int_sampler_buffer, `Sampler; 
      Gl.unsigned_int_sampler_2d_rect, `Sampler; ]
        
  let attr_type_of_enum =
    Gl_hi.enum_map ~not_found:(fun e -> `Unsupported e) [
      Gl.float, `Float32 `V1;
      Gl.float_vec2, `Float32 `V2; 
      Gl.float_vec3, `Float32 `V3; 
      Gl.float_vec4, `Float32 `V4; 
      Gl.float_mat2, `Float32 `M2; 
      Gl.float_mat3, `Float32 `M3; 
      Gl.float_mat4, `Float32 `M4; 
      Gl.float_mat2x3, `Float32 `M23; (* TODO remove these *) 
      Gl.float_mat2x4, `Float32 `M24; 
      Gl.float_mat3x2, `Float32 `M32; 
      Gl.float_mat3x4, `Float32 `M34; 
      Gl.float_mat4x2, `Float32 `M42; 
      Gl.float_mat4x3, `Float32 `M43; 
      Gl.int, `Int32 `V1; 
      Gl.int_vec2, `Int32 `V1; 
      Gl.int_vec3, `Int32 `V3; 
      Gl.int_vec4, `Int32 `V4; 
      Gl.unsigned_int, `UInt32 `V1; 
      Gl.unsigned_int_vec2, `UInt32 `V2; 
      Gl.unsigned_int_vec3, `UInt32 `V3; 
      Gl.unsigned_int_vec4, `UInt32 `V4 ]

  let enum_of_shader_stage = function
  | `Fragment -> Gl.fragment_shader 
  | `Vertex -> Gl.vertex_shader 
  | `Geometry -> Gl.geometry_shader
  | _ -> failwith "TODO Gl 4."

  type attr_binfo = 
    { a_name : string;
      a_loc : int; 
      a_type : attr_type; 
      a_size : int; }
      
  type uniform_binfo = 
    { u_name : string; 
      u_loc : int; 
      u_type : uniform_type; 
      u_size : int; }

  type binfo = 
    { id : Id.t;                      (* associated GL program id. *) 
      attrs : attr_binfo Smap.t;    (* maps attribute names to their info. *) 
      uniforms : uniform_binfo Smap.t; (* maps uniform names to their info. *) }

  let error_binfo = (* info for failing programs. *) 
    { id = 0; attrs = Smap.empty; uniforms = Smap.empty; } 

  module BVal = struct
    type t = binfo 
    let finalizer_add_action _ i = fun () -> 
      if i.id <> 0 then Gl.delete_program i.id
  end

  include BInfo_handler (Prog) (BVal)

  (* Used by setup_binfo, avoid allocating them all the time *) 
  let size_cell = Ba.create Ba.Int32 1
  let type_cell = Ba.create Ba.Int32 1

  let setup_binfo r prog id =     (* lookup attributes and uniform specs *) 
    let cell_val a = Int32.to_int a.{0} in
    let len1 = Gl_hi.get_program_int id Gl.active_attribute_max_length in
    let len2 = Gl_hi.get_program_int id Gl.active_uniform_max_length in 
    let len = max len1 len2 + 1 in
    let name = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in 
    let a_count = Gl_hi.get_program_int id Gl.active_attributes in 
    let attrs = ref Smap.empty in
    for i = 0 to a_count - 1 do
      Gl.get_active_attrib id i len None size_cell type_cell name; 
      let a_name = Gl.string_of_bigarray name in 
      let a_loc = Gl.get_attrib_location id a_name in
      let a_type = attr_type_of_enum (cell_val type_cell) in
      let a_size = cell_val size_cell in
      (* TODO should we say something about supported types ? *) 
      let binfo = { a_name; a_loc; a_type; a_size } in
      attrs := Smap.add a_name binfo !attrs; 
    done;
    let u_count = Gl_hi.get_program_int id Gl.active_uniforms in 
    let uniforms = ref Smap.empty in
    for i = 0 to u_count - 1 do
      Gl.get_active_uniform id i len None size_cell type_cell name; 
      let u_name = Gl.string_of_bigarray name in 
      let u_type = uniform_type_of_enum (cell_val type_cell) in
      let u_size = cell_val size_cell in 
      if u_size = 1 then begin 
        let u_loc = Gl.get_uniform_location id u_name in
        let binfo = { u_name; u_loc; u_size; u_type } in
        uniforms := Smap.add u_name binfo !uniforms
      end else begin 
        failwith "TODO handle array/struct uniforms"
      end
    done;
    let i = { id; attrs = !attrs; uniforms = !uniforms } in 
    set_binfo r prog i;
    i
    
  let compiler_log_msg r sid file_id_map =
    let log = Gl_hi.get_shader_info_log sid in
    Log.compiler_msg log r.compiler_msg_parser file_id_map

  let linker_log_msg r id = 
    let log = Gl_hi.get_program_info_log id in
    `Linker (Log.lines log)

  let compile r acc shader = match acc with 
  | `Error -> `Error 
  | `Ok ids -> 
      let lang = match Prog.lang shader with 
      | None -> Some (`GLSL 150) 
      | Some _ as lang -> lang
      in
      let src, fmap = Prog.source ?lang shader in
      let stage = enum_of_shader_stage (Prog.stage shader) in
      let id = Gl.create_shader stage in
      let ids = `Ok (id :: ids) in
      Gl.shader_source id src;
      Gl.compile_shader id;
      if Gl_hi.get_shader_int id Gl.compile_status = Gl.true_ then ids else
      (log_error r (compiler_log_msg r id fmap); Gl.delete_shader id; `Error)
  
  let setup r prog = match binfo prog with
  | Some i -> if i.id = 0 then `Error else `Ok i.id
  | None -> 
      let supported = r.caps.Cap.c_shader_stages in
      let supported s = List.mem (Prog.stage s) supported in 
      let shaders, unsupported = List.partition supported (Prog.shaders prog) in
      if unsupported <> [] 
      then (log_error r (`Unsupported_shaders (prog, unsupported)); `Error)
      else
      let binfo = match List.fold_left (compile r) (`Ok []) shaders with 
      | `Ok ids ->
          let id = Gl.create_program () in 
          let attach sid = Gl.attach_shader id sid; Gl.delete_shader sid in
          List.iter attach ids;
          Gl.link_program id;
          if Gl_hi.get_program_int id Gl.link_status = Gl.true_ 
          then `Ok (setup_binfo r prog id) 
          else 
          begin 
            log_error r (linker_log_msg r id); 
            Gl.delete_program id; 
            `Error
          end
      | `Error -> `Error 
      in
      match binfo with 
      | `Error -> set_binfo r prog error_binfo; `Error
      | `Ok binfo -> `Ok (binfo.id)

  let resolve_builtin r model_to_world = function 
  | `Model_to_world -> `M4 (Lazy.force model_to_world)
  | `Model_to_view -> `M4 (M4.mul (View.tr r.view) (Lazy.force model_to_world))
  | `Model_to_clip -> 
      `M4 (M4.mul (Lazy.force r.world_to_clip) (Lazy.force model_to_world))
  | `Model_normal_to_view -> 
      `M3 (M3.of_m4 
             (M4.(transpose (inv (mul (View.tr r.view)
                                    (Lazy.force model_to_world))))))
  | `World_to_clip -> `M4 (Lazy.force r.world_to_clip)
  | `World_to_view -> `M4 (View.tr r.view)
  | `View_to_clip -> `M4 (View.proj r.view)
  | `Viewport_size -> `V2 (V2.mul (Box2.size (View.viewport r.view)) r.size) 
  | `Viewport_o -> `V2 (V2.mul (Box2.o (View.viewport r.view)) r.size)

  let ivec k = function 
  | `Bool b -> if k > 0 then raise Exit else (if b then 1 else 0)
  | `Int i -> if k > 0 then raise Exit else i
  | `Float f -> if k > 0 then raise Exit else (Float.int_of_round f)
  | `V2 v ->
      begin match k with 
      | 0 -> Float.int_of_round (V2.x v)
      | 1 -> Float.int_of_round (V2.y v)
      | k -> raise Exit
      end
  | `V3 v ->
      begin match k with 
      | 0 -> Float.int_of_round (V3.x v)
      | 1 -> Float.int_of_round (V3.y v)
      | 2 -> Float.int_of_round (V3.z v)
      | k -> raise Exit
      end
  | `V4 v -> 
      begin match k with 
      | 0 -> Float.int_of_round (V4.x v)
      | 1 -> Float.int_of_round (V4.y v)
      | 2 -> Float.int_of_round (V4.z v)
      | 3 -> Float.int_of_round (V4.w v)
      | _ -> assert false
      end
  | _ -> raise Exit

  let fvec k = function
  | `Bool b -> if k > 0 then raise Exit else (if b then 1. else 0.)
  | `Int i -> if k > 0 then raise Exit else (float i)
  | `Float f -> if k > 0 then raise Exit else f
  | `V2 v ->
      begin match k with 
      | 0 -> V2.x v
      | 1 -> V2.y v
      | k -> raise Exit
      end
  | `V3 v ->
      begin match k with 
      | 0 -> V3.x v
      | 1 -> V3.y v
      | 2 -> V3.z v
      | k -> raise Exit
      end
  | `V4 v -> 
      begin match k with 
      | 0 -> V4.x v
      | 1 -> V4.y v
      | 2 -> V4.z v
      | 3 -> V4.w v
      | _ -> assert false
      end
  | _ -> raise Exit 
      
  let mc = Ba.create Ba.Float32 16
  let m2 = function 
  | `M2 m -> 
      let open M2 in
      mc.{0} <- e00 m; mc.{3} <- e01 m;
      mc.{1} <- e10 m; mc.{4} <- e11 m;
      mc
  | _ -> raise Exit 

  let m3 = function 
  | `M3 m -> 
      let open M3 in
      mc.{0} <- e00 m; mc.{3} <- e01 m; mc.{6} <- e02 m;
      mc.{1} <- e10 m; mc.{4} <- e11 m; mc.{7} <- e12 m;
      mc.{2} <- e20 m; mc.{5} <- e21 m; mc.{8} <- e22 m;
      mc
  | _ -> raise Exit 

  let m4 = function 
  | `M4 m -> 
      let open M4 in
      mc.{0} <- e00 m; mc.{4} <- e01 m; mc.{ 8} <- e02 m; mc.{12} <- e03 m;
      mc.{1} <- e10 m; mc.{5} <- e11 m; mc.{ 9} <- e12 m; mc.{13} <- e13 m;
      mc.{2} <- e20 m; mc.{6} <- e21 m; mc.{10} <- e22 m; mc.{14} <- e23 m;
      mc.{3} <- e30 m; mc.{7} <- e31 m; mc.{11} <- e32 m; mc.{15} <- e33 m;
      mc
  | _ -> raise Exit 

  let tex = function 
  | `Tex t -> t
  | _ -> raise Exit

  let bind_uniforms r prog op =
    let model_to_world = lazy (M4.mul op.tr (Prim.tr op.prim)) in
    let next_active_tex = ref 0 in
    let bind_uniform name u = (* TODO improve *) 
      let v = match Uniform.find_named op.uniforms name with 
      | None -> Uniform.find_named (Effect.uniforms op.effect) name 
      | Some _ as v -> v
      in
      match v with
      | None -> log_error r (`Msg (str "uniform %s undefined" name))(*TODO*) 
      | Some v ->
          let v = match v with 
          | `Builtin b -> resolve_builtin r model_to_world b 
          | v -> v 
          in 
          let i = ivec in
          let f = fvec in 
          try match u.u_type with 
          | `Int32 dim | `Bool dim ->
              begin match dim with 
              | `V1 -> Gl.uniform1i u.u_loc (i 0 v)
              | `V2 -> Gl.uniform2i u.u_loc (i 0 v) (i 1 v)
              | `V3 -> Gl.uniform3i u.u_loc (i 0 v) (i 1 v) (i 2 v)
              | `V4 -> Gl.uniform4i u.u_loc (i 0 v) (i 1 v) (i 2 v) (i 3 v)
              end 
          | `UInt32 dim -> 
              begin match dim with 
              | `V1 -> Gl.uniform1ui u.u_loc (i 0 v)
              | `V2 -> Gl.uniform2ui u.u_loc (i 0 v) (i 1 v)
              | `V3 -> Gl.uniform3ui u.u_loc (i 0 v) (i 1 v) (i 2 v)
              | `V4 -> Gl.uniform4ui u.u_loc (i 0 v) (i 1 v) (i 2 v) (i 3 v)
              end
          | `Float32 dim ->
              begin match dim with 
              | `V1 -> Gl.uniform1f u.u_loc (f 0 v)
              | `V2 -> Gl.uniform2f u.u_loc (f 0 v) (f 1 v)
              | `V3 -> Gl.uniform3f u.u_loc (f 0 v) (f 1 v) (f 2 v)
              | `V4 -> Gl.uniform4f u.u_loc (f 0 v) (f 1 v) (f 2 v) (f 3 v)
              | `M2 -> Gl.uniform_matrix2fv u.u_loc 1 false (m2 v) 
              | `M3 -> Gl.uniform_matrix3fv u.u_loc 1 false (m3 v) 
              | `M4 -> Gl.uniform_matrix4fv u.u_loc 1 false (m4 v) 
              end
          | `Sampler ->
              let t = tex v in
              if t == Tex.nil then 
                log_error r (`Msg (str "%s uniform value is nil texture" name))
              else
              let tid = BTex.setup r t in
              let target = BTex.target_enum_of_kind (Tex.kind t) in
              Gl.active_texture (Gl.texture0 + !next_active_tex); 
              Gl.bind_texture target tid;
              Gl.uniform1i u.u_loc !next_active_tex; 
              incr next_active_tex;
          | `Unsupported v -> 
              log_error r (`Msg (str "Unsupported uniform type: %d" v))
          with Exit -> 
            log_error r (`Msg (str "uniform value type mismatch %s" name))
            (*TODO custom error message *)
    in
    Smap.iter bind_uniform prog.uniforms 
    
  let bind_prim r prog_binfo prim =
    let prim_binfo = BPrim.get_binfo prim in    
    Gl.bind_vertex_array prim_binfo.BPrim.id;
    if prim_binfo.BPrim.last_prog = prog_binfo.id then () else
    begin
      let bind_attr name attr_binfo = 
        let loc = try Some (Smap.find name prim_binfo.BPrim.locs) with
        | Not_found -> None
        in
        match loc with
        | Some loc when loc = attr_binfo.a_loc -> () 
        | Some _ | None -> 
            match Prim.find prim name with 
            | None -> r.log `Error (`Missing_attr (prim, name))
            | Some attr -> 
                (* TODO on debug we could try to match attr_binfo.a_type with 
                   attr's type. *) 
                let buf = Attr.buf attr in
                let buf_binfo = BBuf.get_binfo buf in
                let loc = attr_binfo.a_loc in
                let dim = Attr.dim attr in
                let scalar_type = buf_binfo.BBuf.scalar_type in
                let n = Attr.normalize attr in 
                let bytes = Ba.scalar_type_byte_count (Buf.scalar_type buf) in
                let stride = Attr.stride attr * bytes in 
                let first = `Offset (Attr.first attr * bytes) in 
                let int_attr = match attr_binfo.a_type with 
                | `UInt32 _ | `Int32 _ -> true 
                | `Unsupported _ | `Float32 _ -> false
                in
                Gl.bind_buffer Gl.array_buffer buf_binfo.BBuf.id; 
                Gl.enable_vertex_attrib_array loc; 
                if int_attr 
                then Gl.vertex_attrib_ipointer loc dim scalar_type stride first
                else Gl.vertex_attrib_pointer loc dim scalar_type n stride first
      in
      Smap.iter bind_attr prog_binfo.attrs; 
      Gl.bind_buffer Gl.array_buffer 0;
      prim_binfo.BPrim.last_prog <- prog_binfo.id;
    end
   
  let use r id = 
    Gl.use_program id;
end

(* Backend effects *) 

module BEffect = struct

  let enum_of_raster_face_cull = function 
  | `Front -> Gl.front 
  | `Back -> Gl.back 

  let set_raster_state r = match r.Effect.raster_face_cull with 
  | None -> Gl.disable Gl.cull_face_enum
  | Some cull -> 
      Gl.enable Gl.cull_face_enum; 
      Gl.cull_face (enum_of_raster_face_cull cull)

  let enum_of_depth_test = function 
  | `Less ->  Gl.less
  | `Lequal -> Gl.lequal 
  | `Never -> Gl.never
  | `Equal -> Gl.equal
  | `Greater -> Gl.greater
  | `Nequal -> Gl.notequal
  | `Gequal -> Gl.gequal
  | `Always -> Gl.always

  let set_depth_state d =
    Gl.depth_mask d.Effect.depth_write;
    match d.Effect.depth_test with 
    | None -> Gl.disable Gl.depth_test
    | Some test -> 
        Gl.enable Gl.depth_test;
        Gl.depth_func (enum_of_depth_test test); 
        let factor, units = d.Effect.depth_offset in 
        if factor = 0. && units = 0. then Gl.disable Gl.polygon_offset_fill else
        begin 
          Gl.enable Gl.polygon_offset_fill; 
          Gl.polygon_offset factor units
        end

  let enum_of_blend_mul = function 
  | `Zero -> Gl.zero
  | `One -> Gl.one
  | `Src -> Gl.src_color
  | `One_minus_src -> Gl.one_minus_src_color 
  | `Src_a -> Gl.src_alpha
  | `One_minus_src_a -> Gl.one_minus_src_alpha
  | `Src_a_saturate -> Gl.src_alpha_saturate
  | `Src1 -> Gl.src1_color 
  | `One_minus_src1 -> Gl.one_minus_src1_color 
  | `Src1_a -> Gl.src1_alpha
  | `One_minus_src1_a -> Gl.one_minus_src1_alpha
  | `Dst -> Gl.dst_color 
  | `One_minus_dst -> Gl.one_minus_dst_color 
  | `Dst_a -> Gl.dst_alpha 
  | `One_minus_dst_a -> Gl.one_minus_dst_alpha 
  | `Cst -> Gl.constant_color 
  | `One_minus_cst -> Gl.one_minus_constant_color
  | `Cst_a  -> Gl.constant_alpha
  | `One_minus_cst_a -> Gl.one_minus_constant_alpha

  let enums_of_blend_eq = function 
  | `Add (a, b) -> 
      Gl.func_add, enum_of_blend_mul a, enum_of_blend_mul b 
  | `Sub (a, b) -> 
      Gl.func_subtract, enum_of_blend_mul a, enum_of_blend_mul b
  | `Rev_sub (a, b) -> 
      Gl.func_reverse_subtract, enum_of_blend_mul a, enum_of_blend_mul b
  | `Min -> 
      Gl.min, Gl.one (* irrelevant *), Gl.one (* irrelevant *) 
  | `Max -> 
      Gl.max, Gl.one (* irrelevant *), Gl.one (* irrelevant *)

  let set_blend_state b = 
    if not b.Effect.blend then Gl.disable Gl.blend else
    begin
      Gl.enable Gl.blend;
      let eq_rgb, a_rgb, b_rgb = enums_of_blend_eq b.Effect.blend_rgb in 
      let eq_a, a_a, b_a = enums_of_blend_eq b.Effect.blend_a in
      let cst = b.Effect.blend_cst in
      Gl.blend_equation_separate eq_rgb eq_a;
      Gl.blend_func_separate a_rgb b_rgb a_a b_a;
      Color.(Gl.blend_color (r cst) (g cst) (b cst) (a cst))
    end
    
  let set_state r e = 
    let raster = Effect.raster e in 
    let depth = Effect.depth e in 
    let blend = Effect.blend e in 
    if r.raster != raster then (r.raster <- raster; set_raster_state raster);
    if r.depth != depth then (r.depth <- depth; set_depth_state depth);
    if r.blend != blend then (r.blend <- blend; set_blend_state blend);
    ()
end

module BFbuf = struct

  module Rbuf_impl = struct 

    type binfo = { id : Id.t; }    (* Associated GL render buffer object id. *) 

    module BVal = struct 
      type t = binfo 
      let finalizer_add_action _ i = fun () -> 
        if i.id <> 0 then Gl_hi.delete_render_buffer i.id
    end

    include BInfo_handler (Fbuf.Rbuf) (BVal)

    let setup r b = match binfo b with 
    | Some binfo -> binfo.id
    | None -> 
        let id = Gl_hi.gen_render_buffer () in
        let w = Float.int_of_round (Size2.w (Fbuf.Rbuf.size2 b)) in 
        let h = Float.int_of_round (Size2.h (Fbuf.Rbuf.size2 b)) in 
        let sf = Fbuf.Rbuf.sample_format b in
        let f = BTex.internal_format_enum_of_format sf in
        let m = match Fbuf.Rbuf.multisample b with None -> 0 | Some m -> m in
        set_binfo r b { id; };
        Gl.bind_renderbuffer Gl.renderbuffer id;
        Gl.renderbuffer_storage_multisample Gl.renderbuffer m f w h; 
        Gl.bind_renderbuffer Gl.renderbuffer 0;
        id
  end

  type binfo = { id : Id.t; }         (* Associated GL framebuffer object id. *)

  module BVal = struct 
    type t = binfo 
    let finalizer_add_action _ i = fun () -> 
      if i.id <> 0 then Gl_hi.delete_framebuffer i.id
    end

  include BInfo_handler (Fbuf) (BVal)

  let attach r a = 
    let attach pt = function 
    | `Rbuf b -> 
        let bid = Rbuf_impl.setup r b in
        Gl.framebuffer_renderbuffer Gl.framebuffer pt Gl.renderbuffer bid
    | `Tex (level, t) ->
        let tid = BTex.setup r t in
        Gl.framebuffer_texture Gl.framebuffer pt tid level
    | `Tex_layer (level, layer, t) ->
        let tid = BTex.setup r t in 
        Gl.framebuffer_texture_layer Gl.framebuffer pt tid level layer
    in
    match a with 
    | `Color (n, i) -> attach (Gl.color_attachment0 + n) i
    | `Depth i -> attach Gl.depth_attachment i
    | `Depth_stencil i -> attach Gl.depth_stencil_attachment i 
    | `Stencil i -> attach Gl.stencil i

  let rec setup r fb = 
    if fb == Fbuf.default then 0 else
    match binfo fb with 
    | Some binfo -> binfo.id
    | None -> 
        let id = Gl_hi.gen_framebuffer () in
        set_binfo r fb { id; }; 
        Gl.bind_framebuffer Gl.framebuffer id;
        List.iter (attach r) (Fbuf.attachements fb);
        Gl.bind_framebuffer Gl.framebuffer (setup r r.fbuf);
        id

  let status_enum_to_variant = 
    Gl_hi.enum_map ~not_found:(fun e -> failwith "TODO") 
      [ Gl.framebuffer_complete, `Complete;
        Gl.framebuffer_undefined, `Undefined; 
        Gl.framebuffer_incomplete_attachment, `Incomplete_attachement; 
        Gl.framebuffer_incomplete_missing_attachment, 
        `Incomplete_missing_attachement; 
        Gl.framebuffer_incomplete_draw_buffer, `Incomplete_draw_buffer; 
        Gl.framebuffer_incomplete_read_buffer, `Incomplete_read_buffer; 
        Gl.framebuffer_unsupported, `Unsupported; 
        Gl.framebuffer_incomplete_multisample, `Incomplete_multisample; 
        Gl.framebuffer_incomplete_layer_targets, `Incomplete_layer_targets; ]
     
  let raw_clear clears = 
    let clear_mask = ref 0 in
    begin match clears.Fbuf.clear_color with 
    | None -> () 
    | Some c -> 
        Gl.clear_color (Color.r c) (Color.g c) (Color.b c) (Color.a c);
        clear_mask := Gl.(!clear_mask + color_buffer_bit)
    end;
    begin match clears.Fbuf.clear_depth with 
    | None -> () 
    | Some d -> 
        Gl.clear_depth 1.; 
        clear_mask := Gl.(!clear_mask + depth_buffer_bit)
    end;
    begin match clears.Fbuf.clear_stencil with
    | None -> () 
    | Some s -> 
        Gl.clear_stencil s; 
        clear_mask := Gl.(!clear_mask + stencil_buffer_bit)
    end;
    if !clear_mask <> 0 then Gl.clear !clear_mask
        
  let clear r fb = 
    if r.fbuf == fb then raw_clear (Fbuf.clears fb) else 
    let id = setup r fb in 
    Gl.bind_framebuffer Gl.framebuffer id; 
    raw_clear (Fbuf.clears fb); 
    Gl.bind_framebuffer Gl.framebuffer (setup r r.fbuf)

  let raw_status () = 
    status_enum_to_variant (Gl.check_framebuffer_status Gl.framebuffer)

  let status r fb =
    if r.fbuf == fb then raw_status () else
    let id = setup r fb in 
    Gl.bind_framebuffer Gl.framebuffer id; 
    let status = raw_status () in  
    Gl.bind_framebuffer Gl.framebuffer (setup r r.fbuf); status

  let raw_read r fb rb box ~first ~w_stride buf = 
    let st = Buf.scalar_type buf in
    let first = first * (Ba.scalar_type_byte_count st) in 
    let st = BBuf.enum_of_scalar_type st in
    let mode c = 
      if fb == Fbuf.default then Gl.back else
      Gl.color_attachment0 + c 
    in
    let fmt, st = match rb with 
    | `Color_r c -> Gl.read_buffer (mode c); Gl.red, st
    | `Color_g c -> Gl.read_buffer (mode c); Gl.green, st
    | `Color_b c -> Gl.read_buffer (mode c); Gl.blue, st
    | `Color_rgb c -> Gl.read_buffer (mode c); Gl.rgb, st
    | `Color_rgba c -> Gl.read_buffer (mode c); Gl.rgba, st
    | `Depth -> Gl.read_buffer Gl.back; Gl.depth_component, st
    | `Depth_stencil -> 
        if Buf.scalar_type buf <> `UInt32 
        then invalid_arg "buffer must be of scalar type `UInt32";
        Gl.read_buffer Gl.back; Gl.depth_stencil, Gl.unsigned_int_24_8
    | `Stencil ->
        Gl.read_buffer Gl.back; Gl.stencil_index, st                          
    in
    let bid = BBuf.setup r buf in
    let x = Float.int_of_round (Box2.ox box) in 
    let y = Float.int_of_round (Box2.oy box) in 
    let w = Float.int_of_round (Box2.w box) in 
    let h = Float.int_of_round (Box2.h box) in
    let row_length = match w_stride with None -> 0 | Some s -> s in
    Gl.pixel_storei Gl.pack_alignment 1; 
    Gl.pixel_storei Gl.pack_row_length row_length; 
    Gl.bind_buffer Gl.pixel_pack_buffer bid; 
    Gl.read_pixels x y w h fmt st (`Offset first);
    Gl.bind_buffer Gl.pixel_pack_buffer 0
  
  let read r fb rb box ~first ~w_stride buf = 
    if r.fbuf == fb then raw_read r fb rb box ~first ~w_stride buf else
    let id = setup r fb in 
    Gl.bind_framebuffer Gl.framebuffer id; 
    raw_read r fb rb box ~first ~w_stride buf;
    Gl.bind_framebuffer Gl.framebuffer (setup r r.fbuf)
end


let init_gl_state r = 
  Gl.enable Gl.scissor_test;
  Gl.enable Gl.blend;
  Gl.blend_equation Gl.func_add; 
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha;
  ()

let init_framebuffer r clear =
  let viewport = View.viewport r.view in
  let o = V2.mul (Box2.o viewport) r.size in
  let ox = Float.int_of_round (P2.x o) in
  let oy = Float.int_of_round (P2.x o) in
  let size = V2.mul (Box2.size viewport) r.size in
  let w = Float.int_of_round (Size2.w size) in 
  let h = Float.int_of_round (Size2.h size) in 
  Gl.viewport ox oy w h; 
  Gl.scissor ox oy w h;
  if clear then BFbuf.clear r r.fbuf;
  ()

let render_op r prog_binfo op = 
  BProg.bind_prim r prog_binfo op.prim; 
  BProg.bind_uniforms r prog_binfo op;
  BEffect.set_state r op.effect;
  match Prim.index op.prim with
  | None -> 
      let prim_binfo = BPrim.get_binfo op.prim in
      let first = Prim.first op.prim in 
      let count = Prim.count_now op.prim in
      let mode = prim_binfo.BPrim.kind in
      if op.count = 1
      then Gl.draw_arrays mode first count
      else Gl.draw_arrays_instanced mode first count op.count
  | Some index ->
      let prim_binfo = BPrim.get_binfo op.prim in 
      let index_binfo = BBuf.get_binfo index in
      let first = `Offset (Prim.first op.prim) in 
      let count = Prim.count_now op.prim in 
      let mode = prim_binfo.BPrim.kind in
      let type_ = index_binfo.BBuf.scalar_type in
      if op.count = 1
      then Gl.draw_elements mode count type_ first
      else Gl.draw_elements_instanced mode count type_ first op.count
          
let render_batch r id batch =
  let prog_binfo = BProg.get_binfo (Effect.prog (List.hd batch).effect) in
  BProg.use r id; List.iter (render_op r prog_binfo) batch
   
(* Renderer *) 

let name = "Lit %%VERSION%% GL 3.x renderer"

let init r =
  init_gl_state r; 
  BEffect.set_raster_state r.raster;
  BEffect.set_depth_state r.depth;
  BEffect.set_blend_state r.blend;
  ()

let create ?compiler_msg_parser log ~debug size = 
  let compiler_msg_parser = match compiler_msg_parser with 
  | None -> (* FIXME: we could try to adapt the parser to drivers
               we recognize through GL_RENDERER, contributions welcome. *)
      Log.compiler_msg_parser_default 
  | Some parser -> parser
  in
  let r = { caps = BCap.get ();
            debug; log; compiler_msg_parser; 
            cleanups = []; 
            raster = Lit.Effect.raster_default; 
            depth = Lit.Effect.depth_default; 
            blend = Lit.Effect.blend_default;
            fbuf = Fbuf.default;
            size; 
            view = View.create ();
            world_to_clip = lazy M4.id;
            batches = Imap.empty; 
             }
  in
  (init r; r)

let size r = r.size 
let set_size r s = r.size <- s
let view r = r.view
let set_view r view = 
  r.view <- view;
  r.world_to_clip <- lazy (M4.mul (View.proj view) (View.tr view))

let fbuf r = r.fbuf
let set_fbuf r fbuf =
  let id = BFbuf.setup r fbuf in 
  Gl.bind_framebuffer Gl.framebuffer id; 
  r.fbuf <- fbuf


let add_op r op =  match BProg.setup r (Effect.prog op.effect) with 
| `Error -> ()
| `Ok prog_id -> 
    let batches = try Imap.find prog_id r.batches with Not_found -> [] in
    r.batches <- Imap.add prog_id (op :: batches) r.batches; 
    BPrim.setup r op.prim

let cleanup r =         
  List.iter (fun clean -> clean ()) r.cleanups; 
  r.cleanups <- []

let render r ~clear =
  if not (r.cleanups = []) then cleanup r; 
  init_framebuffer r clear;
  let batches = r.batches in 
  r.batches <- Imap.empty;
  Imap.iter (render_batch r) batches;
  Gl_hi.check_error ();
  ()
  
let release r = ()
  
(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:
     
   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

   3. Neither the name of Daniel C. Bünzli nor the names of
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ---------------------------------------------------------------------------*)
