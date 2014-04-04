(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Lit
open Tgl3

let str = Printf.sprintf 

module Id = Renderer.Private.Id
module Imap = Map.Make (Id) 
module Info = Renderer.Private.Info
module Attr = Renderer.Private.Attr
module Log = Renderer.Private.Log 

module Smap = Map.Make (String)

module Gl_hi = struct                         (* Wraps a few Gl functions. *) 

  let get_string len =
    let a = Ba.create Bigarray.char len in
    fun f -> f a; Gl.string_of_bigarray a

  let get_int =
    let a = Ba.create Bigarray.int32 1 in
    fun f -> f a; Int32.to_int a.{0}
        
  let set_int = 
    let a = Ba.create Bigarray.int32 1 in 
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
      Printf.eprintf "%s: %s%!" loc (error_to_string e)
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
  { mutable init : bool;
    mutable debug : bool;
    mutable log : Log.t; 
    mutable compiler_msg_parser : Log.compiler_msg_parser;
    mutable clears : Renderer.clears;
    mutable size : size2;
    mutable view : view;
    mutable world_to_clip : m4 Lazy.t; (* cache *) 
    mutable 
      batches : op list Imap.t; (* maps programs ids to rendering operations *) 
  }

let log_error r msg = r.log `Error msg 
let log_debug r msg = r.log `Debug msg

(* Caps *) 

module Cap = struct
  include Renderer.Private.Cap

  (* Shader capabilities *) 

  let shader_kinds = [ `Vertex; `Fragment; `Geometry ]
  let shader_kinds r = shader_kinds

  (* OpenGL information *) 

  let get_version r enum = match Gl.get_string enum with 
  | None -> `Unknown 
  | Some v -> 
      match parse_version v with 
      | Some (x,y,z) -> `GL (x, y, z)
      | None -> `Unknown 
    
  let gl_version r = get_version r Gl.version 
  let glsl_version r = get_version r Gl.shading_language_version 
  let gl_renderer r = match Gl.get_string Gl.renderer with 
  | None -> "unknown" | Some r -> r

  let gl_vendor r = match Gl.get_string Gl.vendor with 
  | None -> "unknown" | Some r -> r    
end

(* Bufs *) 

module Buf = struct
  include Renderer.Private.Buf

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
                       
  type info = 
    { id : Id.t;                       (* Associated GL buffer object id. *) 
      scalar_type : Gl.enum;           (* Buffer scalar type *) }

  let inject, project = Info.create () 
  let info b = project (info b)
  let get_info b = match info b with None -> assert false | Some i -> i
  let set_info b (i : info) = set_info b (inject i)

  let finalise_info i =
    Gl_hi.delete_buffer i.id

  let setup_info r b id = 
    let i = { id; scalar_type = enum_of_scalar_type (Buf.scalar_type b) } in 
    Gc.finalise finalise_info i;
    set_info b i; 
    i

  let setup r b = 
    if not (Buf.gpu_upload b) then () else
    let id = match info b with 
    | Some info -> info.id
    | None -> 
        let id = Gl_hi.gen_buffer () in
        let info = setup_info r b id in
        info.id
    in
    let usage = enum_of_usage (Buf.usage b) in
    let byte_count = cpu_byte_count b in
    Gl.bind_buffer Gl.array_buffer id; 
    Gl.buffer_data Gl.array_buffer 0 None usage; (* mark buffer as unused. *) 
    begin match cpu_p b with 
    | None -> Gl.buffer_data Gl.array_buffer byte_count None usage
    | Some (Ba ba) -> Gl.buffer_data Gl.array_buffer byte_count (Some ba) usage
    end;
    Gl.bind_buffer Gl.array_buffer 0; 
    set_gpu_count b (Buf.cpu_count b);
    set_gpu_exists b true;
    Buf.set_gpu_upload b false;
    if Buf.cpu_autorelease b then Buf.set_cpu b None

  (* Mapping the GPU buffer. 
     
     FIXME: Improve safety. What could be done is keep a ref 
     on the returned big array in info, and set its length to 
     zero once we unmap (would need to do it in C I guess).  *)

  let map r access b kind = 
    setup r b; 
    check_kind b kind;
    let info = get_info b in 
    let access = enum_of_access access in
    let count = Buf.gpu_count b in 
    Gl.bind_buffer Gl.array_buffer info.id; 
    let ba = Gl.map_buffer Gl.array_buffer count access kind in 
    Gl.bind_buffer Gl.array_buffer 0;
    ba

  let unmap r b = 
    setup r b;
    let info = get_info b in
    Gl.bind_buffer Gl.array_buffer info.id;
    ignore (Gl.unmap_buffer Gl.array_buffer); 
    Gl.bind_buffer Gl.array_buffer 0
end

(* Prims *) 

module Prim = struct
  include Renderer.Private.Prim
            
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
                              
  type info =                     
    { id : Id.t;                 (* Associated GL vertex array object. *) 
      kind : Gl.enum;                  (* kind of primitive (GL mode). *) 
      mutable locs : int Smap.t; (* maps attr names to bound location. *) 
      mutable last_prog : Id.t; }  (* last program id it was bound to. *) 
          
  let inject, project = Info.create ()
  let info p = project (info p)
  let get_info p = match info p with None -> assert false | Some i -> i
  let set_info p (i : info) = set_info p (inject i)

  let finalise_info i =
    Gl_hi.delete_vertex_array i.id

  let setup_info r p id =
    let kind = enum_of_kind (kind p) in
    let i = { id; kind; locs = Smap.empty; last_prog = 0 } in 
    Gc.finalise finalise_info i;
    set_info p i;
    i

  let setup r p = match info p with 
  | Some _ -> 
      let update_attr a = Buf.setup r (Attr.buf a) in
      Prim.iter update_attr p; 
      begin match index p with 
      | None -> () 
      | Some b -> Buf.setup r b
      end
  | None -> 
      let id = Gl_hi.gen_vertex_array () in
      let info = setup_info r p id in
      let index_id = match index p with 
      | None -> 0 
      | Some index -> Buf.setup r index; (Buf.get_info index).Buf.id
      in
      Prim.iter (fun a -> Buf.setup r (Attr.buf a)) p;
      Gl.bind_vertex_array info.id;
      Gl.bind_buffer Gl.element_array_buffer index_id;
      (* attrs are bound later, see Prog.bind_prim *) 
      Gl.bind_vertex_array 0; 
      Gl.bind_buffer Gl.element_array_buffer 0
end

(* Textures *) 

module Tex = struct
  include Renderer.Private.Tex

  let target_enum_of_kind = function 
  | `D1 -> Gl.texture_1d 
  | `D2 -> Gl.texture_2d 
  | `D3 -> Gl.texture_3d 
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

  type info = { id : Id.t;  }
  let inject, project = Info.create () 
  let info p = project (info p) 
  let get_info p = match info p with None -> assert false | Some i -> i 
  let set_info p (i : info) = set_info p (inject i) 

  let finalise_info i = 
    Gl_hi.delete_texture i.id

  let setup_info r t id = 
    let i = { id } in 
    Gc.finalise finalise_info i;
    set_info t i; 
    i

  let setup r t = 
    if t == Tex.nil then () else
    let id = match info t with
    | Some info -> info.id
    | None -> 
        let id = Gl_hi.gen_texture () in 
        let info = setup_info r t id  in
        info.id
    in
    if not (Tex.gpu_update t) then () else
    let buf_id = match Tex.buf t with 
    | None -> 0 
    | Some b -> Buf.setup r b; (Buf.get_info b).Buf.id 
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
    | `Buffer -> 
        Gl.tex_buffer target internal buf_id
    end;
    if kind <> `Buffer then begin
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
    ()
end

(* Programs *) 

module Prog = struct
  include Renderer.Private.Prog 

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

  let enum_of_shader_kind = function
  | `Fragment -> Gl.fragment_shader 
  | `Vertex -> Gl.vertex_shader 
  | `Geometry -> Gl.geometry_shader
  | _ -> assert false

  type attr_info = 
    { a_name : string;
      a_loc : int; 
      a_type : attr_type; 
      a_size : int; }
      
  type uniform_info = 
    { u_name : string; 
      u_loc : int; 
      u_type : uniform_type; 
      u_size : int; }

  type info = 
    { id : Id.t;                      (* associated GL program id. *) 
      attrs : attr_info Smap.t;       (* maps attribute names to their info. *) 
      uniforms : uniform_info Smap.t; (* maps uniform names to their info. *) }

  let error_info = (* info for failing programs. *) 
    { id = 0; attrs = Smap.empty; uniforms = Smap.empty; } 

  let inject, project = Info.create ()
  let info prog = project (info prog)
  let get_info prog = match info prog with None -> assert false | Some i -> i
  let set_info prog (i : info) = set_info prog (inject i)

  let finalise_info i =
    Gl.delete_program i.id

  (* Used by setup_info, avoid allocating them all the time *) 
  let size_cell = Ba.create Bigarray.int32 1
  let type_cell = Ba.create Bigarray.int32 1

  let setup_info r prog id =     (* lookup attributes and uniform specs *) 
    let cell_val a = Int32.to_int a.{0} in
    let len1 = Gl_hi.get_program_int id Gl.active_attribute_max_length in
    let len2 = Gl_hi.get_program_int id Gl.active_uniform_max_length in 
    let len = max len1 len2 + 1 in
    let name = Ba.create Bigarray.char len in 
    let a_count = Gl_hi.get_program_int id Gl.active_attributes in 
    let attrs = ref Smap.empty in
    for i = 0 to a_count - 1 do
      Gl.get_active_attrib id i len None size_cell type_cell name; 
      let a_name = Gl.string_of_bigarray name in 
      let a_loc = Gl.get_attrib_location id a_name in
      let a_type = attr_type_of_enum (cell_val type_cell) in
      let a_size = cell_val size_cell in
      (* TODO should we say something about supported types ? *) 
      let info = { a_name; a_loc; a_type; a_size } in
      attrs := Smap.add a_name info !attrs; 
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
        let info = { u_name; u_loc; u_size; u_type } in
        uniforms := Smap.add u_name info !uniforms
      end else begin 
        failwith "TODO handle array/struct uniforms"
      end
    done;
    let i = { id; attrs = !attrs; uniforms = !uniforms } in 
    Gc.finalise finalise_info i;
    set_info prog i;
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
      let kind = enum_of_shader_kind (Prog.kind shader) in
      let id = Gl.create_shader kind in
      let ids = `Ok (id :: ids) in
      Gl.shader_source id src;
      Gl.compile_shader id;
      if Gl_hi.get_shader_int id Gl.compile_status = Gl.true_ then ids else
      (log_error r (compiler_log_msg r id fmap); Gl.delete_shader id; `Error)
  
  let setup r prog = match info prog with
  | Some i -> if i.id = 0 then `Error else `Ok i.id
  | None -> 
      let supported = Cap.shader_kinds r in
      let supported s = List.mem (Prog.kind s) supported in 
      let shaders, unsupported = List.partition supported (Prog.shaders prog) in
      if unsupported <> [] 
      then (log_error r (`Unsupported_shaders (prog, unsupported)); `Error)
      else
      let info = match List.fold_left (compile r) (`Ok []) shaders with 
      | `Ok ids ->
          let id = Gl.create_program () in 
          let attach sid = Gl.attach_shader id sid; Gl.delete_shader sid in
          List.iter attach ids;
          Gl.link_program id;
          if Gl_hi.get_program_int id Gl.link_status = Gl.true_ 
          then `Ok (setup_info r prog id) 
          else 
          begin 
            log_error r (linker_log_msg r id); 
            Gl.delete_program id; 
            `Error
          end
      | `Error -> `Error 
      in
      match info with 
      | `Error -> set_info prog error_info; `Error
      | `Ok info -> `Ok (info.id)

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
      
  let mc = Ba.create Bigarray.float32 16
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
              let tid = Tex.setup r t; (Tex.get_info t).Tex.id in
              let target = Tex.target_enum_of_kind (Tex.kind t) in
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
    
  let bind_prim r prog_info prim =
    let prim_info = Prim.get_info prim in    
    Gl.bind_vertex_array prim_info.Prim.id;
    if prim_info.Prim.last_prog = prog_info.id then () else
    begin
      let bind_attr name attr_info = 
        let loc = try Some (Smap.find name prim_info.Prim.locs) with
        | Not_found -> None
        in
        match loc with
        | Some loc when loc = attr_info.a_loc -> () 
        | Some _ | None -> 
            match Prim.find prim name with 
            | None -> r.log `Error (`Missing_attr (prim, name))
            | Some attr -> 
                (* TODO on debug we could try to match attr_info.a_type with 
                   attr's type. *) 
                let buf = Attr.buf attr in
                let buf_info = Buf.get_info buf in
                let loc = attr_info.a_loc in
                let dim = Attr.dim attr in
                let scalar_type = buf_info.Buf.scalar_type in
                let n = Attr.normalize attr in 
                let bytes = Buf.(scalar_type_byte_count (scalar_type buf)) in
                let stride = Attr.stride attr * bytes in 
                let first = `Offset (Attr.first attr * bytes) in 
                let int_attr = match attr_info.a_type with 
                | `UInt32 _ | `Int32 _ -> true 
                | `Unsupported _ | `Float32 _ -> false
                in
                Gl.bind_buffer Gl.array_buffer buf_info.Buf.id; 
                Gl.enable_vertex_attrib_array loc; 
                if int_attr 
                then Gl.vertex_attrib_ipointer loc dim scalar_type stride first
                else Gl.vertex_attrib_pointer loc dim scalar_type n stride first
      in
      Smap.iter bind_attr prog_info.attrs; 
      Gl.bind_buffer Gl.array_buffer 0;
      prim_info.Prim.last_prog <- prog_info.id;
    end
   
  let use r id = 
    Gl.use_program id;
end

(* Effects *) 

module Effect = struct
  include Renderer.Private.Effect

  let set_raster_state r e = match (raster e).raster_cull with 
  | None -> Gl.disable Gl.cull_face_enum
  | Some cull -> 
      Gl.enable Gl.cull_face_enum; 
      match cull with 
      | `Front -> Gl.cull_face Gl.front
      | `Back -> Gl.cull_face Gl.back

  let enum_of_depth_test = function 
  | `Less ->  Gl.less
  | `Lequal -> Gl.lequal 
  | `Never -> Gl.never
  | `Equal -> Gl.equal
  | `Greater -> Gl.greater
  | `Nequal -> Gl.notequal
  | `Gequal -> Gl.gequal
  | `Always -> Gl.always

  let set_depth_state r e =
    let d = depth e in 
    Gl.depth_mask d.depth_write;
    match d.depth_test with 
    | None -> Gl.disable Gl.depth_test
    | Some test -> 
        Gl.enable Gl.depth_test;
        Gl.depth_func (enum_of_depth_test test); 
        let factor, units = d.depth_offset in 
        if factor = 0. && units = 0. then Gl.disable Gl.polygon_offset_fill else
        begin 
          Gl.enable Gl.polygon_offset_fill; 
          Gl.polygon_offset factor units
        end
end

let init_gl_state r = 
  Gl.enable Gl.scissor_test;
  (* Raster *)
  Gl.disable Gl.cull_face_enum;
  (* Depth *)
  Gl.disable Gl.depth_test;
  Gl.disable Gl.polygon_offset_fill;
  Gl.depth_func Gl.less;
  Gl.depth_mask true;
  (* TODO move that to blend state ? *) 
  Gl.enable Gl.blend;
  Gl.blend_equation Gl.func_add; 
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha

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
  if clear then begin 
    let clears = ref 0 in 
    begin match r.clears.Renderer.clear_color with 
    | None -> () 
    | Some c -> 
        Gl.clear_color (Color.r c) (Color.g c) (Color.b c) (Color.a c);
        clears := Gl.(!clears + color_buffer_bit)
    end;
    begin match r.clears.Renderer.clear_depth with 
    | None -> () 
    | Some d -> 
        Gl.clear_depth 1.; 
        clears := Gl.(!clears + depth_buffer_bit)
    end;
    begin match r.clears.Renderer.clear_stencil with 
    | None -> () 
    | Some s -> 
        Gl.clear_stencil s; 
        clears := Gl.(!clears + stencil_buffer_bit)
    end;
    if !clears <> 0 then Gl.clear !clears
  end;
  ()

let render_op r prog_info op = 
  Prog.bind_prim r prog_info op.prim; 
  Prog.bind_uniforms r prog_info op;
  Effect.set_raster_state r op.effect; 
  Effect.set_depth_state r op.effect; 
  match Prim.index op.prim with
  | None -> 
      let prim_info = Prim.get_info op.prim in
      let first = Prim.first op.prim in 
      let count = Prim.count_now op.prim in
      let mode = prim_info.Prim.kind in
      if op.count = 1
      then Gl.draw_arrays mode first count
      else Gl.draw_arrays_instanced mode first count op.count
  | Some index ->
      let prim_info = Prim.get_info op.prim in 
      let index_info = Buf.get_info index in
      let first = `Offset (Prim.first op.prim) in 
      let count = Prim.count_now op.prim in 
      let mode = prim_info.Prim.kind in
      let type_ = index_info.Buf.scalar_type in
      if op.count = 1
      then Gl.draw_elements mode count type_ first
      else Gl.draw_elements_instanced mode count type_ first op.count
      
let render_batch r id batch =
  let prog_info = Prog.get_info (Effect.prog (List.hd batch).effect) in
  Prog.use r id; List.iter (render_op r prog_info) batch
 
(* Renderer.T implementation *) 

let name = "Lit %%VERSION%% GL 3.x renderer"

let create ?compiler_msg_parser log ~debug size = 
  let compiler_msg_parser = match compiler_msg_parser with 
  | None -> 
      (* TODO Potentially we'd like to defer that decision in init () 
         once we have an OpenGL context to recognise drivers and adapt the 
         default to them. *) 
      Log.compiler_msg_parser_default 
  | Some parser -> parser
  in
  { init = false;
    debug; log; compiler_msg_parser; 
    clears = Renderer.default_clears;
    size; 
    view = View.create ();
    world_to_clip = lazy M4.id;
    batches = Imap.empty; }

let init r =
  init_gl_state r; 
  ()

let size r = r.size 
let set_size r s = r.size <- s
let view r = r.view
let set_view r view = 
  r.view <- view;
  r.world_to_clip <- lazy (M4.mul (View.proj view) (View.tr view))

let clears r = r.clears 
let set_clears r clears = r.clears <- clears


let add_op r op =  
  if not r.init then (r.init <- true; init r);
  match Prog.setup r (Effect.prog op.effect) with 
  | `Error -> () 
  | `Ok prog_id -> 
      let batches = try Imap.find prog_id r.batches with Not_found -> [] in
      r.batches <- Imap.add prog_id (op :: batches) r.batches; 
      Prim.setup r op.prim

let render r ~clear =
  if not r.init then (r.init <- true; init r);
  init_framebuffer r clear;
  let batches = r.batches in 
  r.batches <- Imap.empty;
  Imap.iter (render_batch r) batches;
  Gl_hi.check_error ()
  
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
