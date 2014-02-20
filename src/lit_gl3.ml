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
    mutable view : view; 
    mutable size : size2;
    mutable 
      batches : op list Imap.t; (* maps programs ids to rendering operations *) 
  }

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
  | `UInt8 -> Gl.unsigned_byte
  | `Int8 -> Gl.byte
  | `UInt16 -> Gl.unsigned_short
  | `Int16 -> Gl.short
  | `UInt32 -> Gl.unsigned_int
  | `Int32 -> Gl.int
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
    let cpu = cpu_p b in 
    Gl.bind_buffer Gl.array_buffer id; 
    Gl.buffer_data Gl.array_buffer 0 None usage; (* mark buffer as unused. *) 
    Gl.buffer_data Gl.array_buffer byte_count cpu usage;
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
      (r.log `Error (compiler_log_msg r id fmap); Gl.delete_shader id; `Error)
  
  let setup r prog = match info prog with
  | Some i -> if i.id = 0 then `Error else `Ok i.id
  | None -> 
      let supported = Cap.shader_kinds r in
      let supported s = List.mem (Prog.kind s) supported in 
      let shaders, unsupported = List.partition supported (Prog.shaders prog) in
      if unsupported <> [] 
      then (r.log `Error (`Unsupported_shaders (prog, unsupported)); `Error)
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
            r.log `Error (linker_log_msg r id); 
            Gl.delete_program id; 
            `Error
          end
      | `Error -> `Error 
      in
      match info with 
      | `Error -> set_info prog error_info; `Error
      | `Ok info -> `Ok (info.id)

  let resolve_builtin r m2w = function 
  | `Viewport_size -> `V2 r.size
  | `Model_to_world -> `M4 (Lazy.force m2w)
  | `Viewport_o -> 
      failwith "TODO"
  | `Model_to_view -> 
      failwith "TODO"
  (* M4.mul r.world_to_view (Lazy.force m2w) *)
  | `Model_to_clip -> 
      failwith "TODO"
  (* M4.mul (M4.mul r.view_to_clip r.world_to_view) (Lazy.force m2w) *)
  | `Model_normal_to_view -> 
      failwith "TODO"
  (*   M4.transpose(M4.inv(self.world_to_view * m2w)) *) 
  | `World_to_clip ->
      failwith "TODO"
   (* M4.mul r.view_to_clip * r.world_to_view *) 
  | `World_to_view -> 
      failwith "TODO"
  (*       r.world_to_view *)

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

  let bind_uniforms r prog e =
    let us = Effect.uniforms e in 
    let m2w = lazy (M4.id) (* TODO *) in 
(*    let next_active_tex = ref 0 in*)
    let bind_uniform name u = match Uniform.find_named us name with 
    | None -> r.log `Error (`Msg (str "uniform %s undefined" name))(*TODO*) 
    | Some v ->
        let v = match v with `Builtin b -> resolve_builtin r m2w b | v -> v in 
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
            failwith "TODO"
(*
            let t = tex v in 
            Gl.active_texture (Gl.texture0 + !next_active_tex); 
            Gl.bind_texture t.id;
            Gl.uniform1i u.u_loc !next_active_tex; 
            incr next_active_tex;
*)

        | `Unsupported _ -> 
          failwith "TODO"
        with Exit -> 
          r.log 
            `Error (`Msg (str "uniform value type mismatch %s" name)) (*TODO*)
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
  let set_rasterization_state r e = ()
  let set_depth_state r e = ()
end

let init_gl_state r = 
  Gl.disable Gl.cull_face_enum;
  Gl.disable Gl.depth_test;
  (* TODO move that to blend state ? *) 
  Gl.enable Gl.blend;
  Gl.blend_equation Gl.func_add; 
  Gl.blend_func Gl.src_alpha Gl.one_minus_src_alpha

let init_framebuffer r = 
  let w = Float.int_of_round (Size2.w r.size) in 
  let h = Float.int_of_round (Size2.h r.size) in
  let c = Color.black in 
  Gl.viewport 0 0 w h;
  Gl.clear_color (Color.r c) (Color.g c) (Color.b c) (Color.a c);
  Gl.clear Gl.color_buffer_bit;
  ()

let render_op r prog_info op = 
  Prog.bind_prim r prog_info op.prim; 
  Prog.bind_uniforms r prog_info op.effect;
  Effect.set_rasterization_state r op.effect; 
  Effect.set_depth_state r op.effect; 
  match Prim.index op.prim with
  | None -> 
      let prim_info = Prim.get_info op.prim in
      let first = Prim.first op.prim in 
      let count = Prim.count op.prim in
      let mode = prim_info.Prim.kind in
      if op.count = 1
      then Gl.draw_arrays mode first count
      else Gl.draw_arrays_instanced mode first count op.count
  | Some index ->
      let prim_info = Prim.get_info op.prim in 
      let index_info = Buf.get_info index in
      let first = `Offset (Prim.first op.prim) in 
      let count = Prim.count op.prim in 
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
    view = View.create ();
    size; 
    batches = Imap.empty; }

let init r =
  init_gl_state r; 
  ()

let size r = r.size 
let set_size r s = r.size <- s
let view r = r.view
let set_view r v = r.view <- v

let frame_begin r = 
  if not r.init then (r.init <- true; init r)
  
let frame_add r op = match Prog.setup r (Effect.prog op.effect) with 
| `Error -> () 
| `Ok prog_id -> 
    let batches = try Imap.find prog_id r.batches with Not_found -> [] in
    r.batches <- Imap.add prog_id (op :: batches) r.batches; 
    Prim.setup r op.prim

let frame_end r =
  init_framebuffer r;
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
