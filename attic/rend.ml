(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

open Gg;;

let required_vers = (1,3) (* TODO fix that number correctly *)
let required_exts = [
  Gl.Ext.arb_vertex_buffer_object;
  Gl.Ext.arb_pixel_buffer_object;
  Gl.Ext.arb_shader_objects; 
  Gl.Ext.arb_vertex_shader; 
(*   Gl.Ext.arb_fragment_shader; TODO remove that *)
  Gl.Ext.arb_shading_language_100; ] 

type opengl_info = 
    { vendor : string; 
      renderer : string; 
      version : string * (int * int); 
      shading_language_version : string * (int * int);
      extensions : string list; }
      
type limits = 
    { max_elements_indices : int;      
      max_elements_vertices : int;     
      max_clip_planes : int;	       
      max_texture_size : int;	       
      max_3d_texture_size : int; 
      max_fragment_uniform_components : int;
      max_fragment_varying_components : int;
      max_fragment_texture_image_units : int;
      max_vertex_attributes : int;
      max_vertex_uniform_components : int;
      max_vertex_texture_image_units : int;
      max_combined_texture_image_units : int; }
      
type caps = 
    { dxt_textures : bool;
      npot_textures : bool; 
      rect_textures : bool;
      float_textures : bool; }

type t =            
    { ft : Gl.Ptr.ft; 
      opengl_info : opengl_info;
      limits : limits;
      caps : caps; }

exception Error of string    
exception Unsupported of opengl_info * string list

let get_opengl () = 
  let supported i = 
    let missing m ext = if List.mem ext i.extensions then m else ext :: m in
    let _, v = i.version in
    match (v < required_vers), (List.fold_left missing [] required_exts) with
    | (false, []) -> i 
    | (_, missing) -> raise (Unsupported (i, missing)) 
  in
  let exts = Gl.lit_extensions () in
  let sl_version = 
    if not (List.mem Gl.Ext.arb_shading_language_100 exts) then 
      "unsupported", (0,0) 
    else
      Gl.lit_version Gl.shading_language_version
  in
  let i =  
    { vendor = Gl.get_string Gl.vendor; 
      renderer = Gl.get_string Gl.renderer; 
      version = Gl.lit_version Gl.version;
      shading_language_version = sl_version;
      extensions = exts }
  in
  supported i

let get_caps exts = 
    let e ext = List.mem ext exts in
    { dxt_textures = e Gl.Ext.ext_texture_compression_s3tc;
      npot_textures = e Gl.Ext.arb_texture_non_power_of_two;
      rect_textures = e Gl.Ext.arb_texture_rectangle;
      float_textures = (e Gl.Ext.arb_texture_float 
			  (* || e Gl.Ext.ati_texture_float *)); }

let get_limits ft = 
  let i = Gl.get_integer ft in
  { max_elements_indices = i Gl.max_elements_indices;
    max_elements_vertices = i Gl.max_elements_vertices;
    max_clip_planes = i Gl.max_clip_planes;
    max_texture_size = i Gl.max_texture_size;
    max_3d_texture_size = i Gl.max_3d_texture_size; 
    max_fragment_uniform_components = i Gl.max_fragment_uniform_components;
    max_fragment_varying_components = i Gl.max_varying_floats;
    max_fragment_texture_image_units = i Gl.max_fragment_texture_image_units;
    max_vertex_attributes = i Gl.max_vertex_attribs;
    max_vertex_uniform_components = i Gl.max_vertex_uniform_components;
    max_vertex_texture_image_units = i Gl.max_vertex_texture_image_units;
    max_combined_texture_image_units = i Gl.max_combined_texture_image_units; }
    
let create () =
  let opengl_info = get_opengl () in
  let ft = Gl.Ptr.fun_table () in
  let caps = get_caps opengl_info.extensions in
  let limits = get_limits ft in
  Gl.pixel_storei ft Gl.unpack_alignment 1;
  Gl.pixel_storei ft Gl.pack_alignment 1;
  { ft = ft;
    opengl_info = opengl_info;
    caps = caps;
    limits = limits }

let _ft r = r.ft 
let opengl_info r = r.opengl_info    
let caps r = r.caps         
let limits r = r.limits
let has_dxt_textures r = r.caps.dxt_textures
let has_npot_textures r = r.caps.npot_textures
let has_rect_textures r = r.caps.rect_textures
let has_float_textures r = r.caps.float_textures
let s b e = if not b then raise (Error (Printf.sprintf "unsupported: %s" e))
let sdxt_textures r = s r.caps.dxt_textures "dxt texture compression"
let snpot_textures r = s r.caps.npot_textures "non power of two textures"
let srect_textures r = s r.caps.rect_textures "rectangle textures"
let sfloat_textures r = s r.caps.float_textures "floating point textures"

let pr_box fmt s a = 
  Format.fprintf fmt "@[<hov 2>%a@]@ " (fun fmt -> Format.fprintf fmt s) a

let print_opengl_info fmt o = 
  let pr s = Format.fprintf fmt s in
  let pr_box = pr_box fmt in
  let v, _ = o.version in
  let slv, _ = o.shading_language_version in
  pr "@[<v>";
  pr_box "vendor =@ %s" o.vendor;
  pr_box "renderer =@ %s" o.renderer;
  pr_box "version =@ %s" v;
  pr_box "shading language version =@ %s" slv; 
  pr "@[<hov 2>extensions =@ @[%a@]@]@ " 
  (fun fmt l -> List.iter (fun e -> pr "%s@ " e) l)
    o.extensions;
  pr "@]"

let print_caps fmt c =
  let pr s = Format.fprintf fmt s in
  let pr_box = pr_box fmt in
  pr "@[<v>";
  pr_box "dxt_textures =@ %b" c.dxt_textures;
  pr_box "npot_textures =@ %b" c.npot_textures;
  pr_box "rect_textures =@ %b" c.rect_textures;
  pr_box "float_textures =@ %b" c.float_textures;
  pr "@]"

let print_limits fmt l = 
  let pr s = Format.fprintf fmt s in
  let pr_box = pr_box fmt in
  pr "@[<v>";
  pr_box "max_elements_indices =@ %d" l.max_elements_indices;
  pr_box "max_elements_vertices =@ %d" l.max_elements_vertices;
  pr_box "max_clip_planes =@ %d" l.max_clip_planes;
  pr_box "max_texture_size =@ %d" l.max_texture_size;
  pr_box "max_3d_texture_size =@ %d" l.max_3d_texture_size;
  pr_box "max_fragment_uniform_components =@ %d" 
    l. max_fragment_uniform_components;
  pr_box "max_fragment_varying_components =@ %d" 
    l.max_fragment_varying_components;
  pr_box "max_fragment_texture_image_units =@ %d" 
    l.max_fragment_texture_image_units;
  pr_box "max_vertex_attributes =@ %d" 
    l.max_vertex_attributes;
  pr_box "max_vertex_uniform_components =@ %d" 
    l.max_vertex_uniform_components;
  pr_box "max_vertex_texture_image_units =@ %d" 
    l.max_vertex_texture_image_units;
  pr_box "max_combined_texture_image_units =@ %d" 
    l.max_combined_texture_image_units;
  pr "@]"

let print_info fmt r =
  Format.fprintf fmt "@[<v>%a%a%a@]" print_opengl_info r.opengl_info 
    print_caps r.caps print_limits r.limits
  

let error r f x = 
  let str e = Printf.sprintf "opengl error : %s" (Gl.lit_error_to_string e) in
  while (Gl.get_error r.ft () <> Gl.no_error) do () done;
  let y = (f x) in
  let e = Gl.get_error r.ft () in
  if e = Gl.no_error then y else raise (Error (str e))

let flush ?(block = false) r = if block then Gl.finish r.ft else Gl.flush r.ft
      
type clear_buf = [ 
  | `Color of v4 | `Depth of float | `Stencil of int32 | `Accum of v4 ]

let clear_fbuf r clears = 
  let b = ref Gl.lit_zero_buffer_bit in
  let set bit = b := Gl.lit_set_buffer_bit !b bit in
  let setup : clear_buf -> unit = function 
    | `Color v -> 
	let c = V4.to_array v in 
	Gl.clear_color r.ft c.(0) c.(1) c.(2) c.(3); 
	set Gl.color_buffer_bit
    | `Depth d -> 
	Gl.clear_depth r.ft d; 
	set Gl.depth_buffer_bit
    | `Stencil s -> 
	Gl.clear_stencil r.ft s; 
	set Gl.stencil_buffer_bit
    | `Accum v ->
	let c = V4.to_array v in 
	Gl.clear_accum r.ft c.(0) c.(1) c.(2) c.(3); 
	set Gl.accum_buffer_bit
  in
  List.iter setup clears;
  if (!b != Gl.lit_zero_buffer_bit) then Gl.clear r.ft !b
	  

(* Geometry stage *)
let set_modelview_matrix r ?(mult = false) m =
  Gl.matrix_mode r.ft Gl.modelview;
  if (mult) then Gl.mult_matrix r.ft m else Gl.load_matrix r.ft m

let set_projection_matrix r ?(mult = false) m = 
  Gl.matrix_mode r.ft Gl.projection;
  if (mult) then Gl.mult_matrix r.ft m else Gl.load_matrix r.ft m

let set_texture_matrix r ?(mult = false) ~unit m = 
  Gl.Cache.active_texture r.ft (Gl.lit_tex_unit unit);
  Gl.Cache.matrix_mode r.ft Gl.texture;
  if (mult) then Gl.mult_matrix r.ft m else Gl.load_matrix r.ft m

(* Window coordinate mapping and clipping *)
let set_viewport r ~o ~size = 
  Gl.viewport r.ft (truncate (V2.x o)) (truncate (V2.y o)) 
    (truncate (V2.x size)) (truncate (V2.y size))

let set_clip_rect r ~o ~size = 
  Gl.scissor r.ft (truncate (V2.x o)) (truncate (V2.y o)) 
    (truncate (V2.x size)) (truncate (V2.y size))

let enable_clip_rect r e = 
  if e then Gl.enable r.ft Gl.scissor_test
  else Gl.disable r.ft Gl.scissor_test

let set_depth_range r ~near ~far = Gl.depth_range r.ft near far

type face_cull = [ `None | `Clockwise | `Counter_clockwise ]

let set_face_cull r = function         (* Invariant front faces are CCW *)
  | `None -> Gl.disable r.ft Gl.cap_cull_face
  | `Clockwise -> Gl.enable r.ft Gl.cap_cull_face; Gl.cull_face r.ft Gl.back
  | `Counter_clockwise -> Gl.enable r.ft Gl.cap_cull_face; Gl.cull_face r.ft Gl.front 


(* Rasterization *)
type shading = [ `Flat | `Smooth ]
		
let set_shading r = function
  | `Flat -> Gl.shade_model r.ft Gl.flat
  | `Smooth -> Gl.shade_model r.ft Gl.smooth 

type raster = [ `Vertices | `Lines | `Faces ]

let set_raster r = function
  | `Vertices -> Gl.polygon_mode r.ft Gl.front_and_back Gl.point
  | `Lines -> Gl.polygon_mode r.ft Gl.front_and_back Gl.line
  | `Faces -> Gl.polygon_mode r.ft Gl.front_and_back Gl.fill

let set_depth_offset r o = 
  let offset f = f Gl.polygon_offset_point;
                 f Gl.polygon_offset_line;
                 f Gl.polygon_offset_fill;
  in
  if (o = 0.) then offset (Gl.disable r.ft)
  else (offset (Gl.enable r.ft) ; Gl.polygon_offset r.ft 1.0 o)


(* Fragment operations *) 
type compare_fun = [ 
  | `False | `Lt | `Eq | `Leq | `Gt | `Neq | `Geq | `True ]

let func_to_enum = function 
  | `False -> Gl.never 
  | `Lt -> Gl.less
  | `Eq -> Gl.equal
  | `Leq -> Gl.lequal
  | `Gt -> Gl.greater
  | `Neq -> Gl.notequal
  | `Geq -> Gl.gequal
  | `True -> Gl.always


(* Alpha test *)

let enable_alpha_test r e = 
  if e then Gl.enable r.ft Gl.alpha_test else Gl.disable r.ft Gl.alpha_test

let set_alpha_test r f ~ref = Gl.alpha_func r.ft (func_to_enum f) ref

(* Stencil test *)

let enable_stencil_test r e = 
  if e then Gl.enable r.ft Gl.stencil_test else Gl.disable r.ft Gl.stencil_test

let enable_stencil_write r w = Gl.stencil_mask r.ft w

let set_stencil_test r f ~ref ~mask = Gl.stencil_func r.ft (func_to_enum f) ref mask

type stencil_update = [
  | `Zero     (** Sets the value to 0. *)
  | `Keep     (** Keeps the current value. *)
  | `Replace  (** Sets the value to the current [ref] value (see 
		 {!Renderer.set_stencil_test}) *)
  | `Incr      (** Increments the current value. Clamped to max. *)
  | `Decr     (** Decrements the current value. Clamps to 0. *)
  | `Invert   (** Bitwise inversion of the current value. *) ]

let stencil_update_to_enum = function (* TODO hash table *)
  | `Zero -> Gl.zero
  | `Keep -> Gl.keep
  | `Replace -> Gl.replace
  | `Incr -> Gl.incr
  | `Decr -> Gl.decr
  | `Invert -> Gl.invert

let set_stencil_update r ~stencil_fail ~depth_fail ~depth_pass = 
  Gl.stencil_op r.ft (stencil_update_to_enum stencil_fail) 
    (stencil_update_to_enum depth_fail) (stencil_update_to_enum depth_pass)


(* Depth test *)
let enable_depth_test r e = 
  if e then Gl.enable r.ft Gl.depth_test else Gl.disable r.ft Gl.depth_test

let enable_depth_write r e = Gl.depth_mask r.ft e

let set_depth_test r f = Gl.depth_func r.ft (func_to_enum f)


(* Color buffer *)
let enable_color_write r' ~r ~g ~b ~a = Gl.color_mask r'.ft r g b a
let enable_color_blend r e = 
  if e then Gl.enable r.ft Gl.blend else Gl.disable r.ft Gl.blend

type dst_blend_factor = [ `Zero | `One | `Src_color 
| `One_minus_src_color | `Dst_color | `One_minus_dst_color | `Src_alpha
| `One_minus_src_alpha | `Dst_alpha | `One_minus_dst_alpha ]

type src_blend_factor = [ dst_blend_factor | `Src_alpha_saturate]

let blend_factor_to_enum = function (* TODO hash table *)
  | `Zero -> Gl.blend_factor_zero
  | `One -> Gl.blend_factor_one
  | `Src_color -> Gl.src_color
  | `One_minus_src_color -> Gl.one_minus_src_color
  | `Dst_color -> Gl.dst_color
  | `One_minus_dst_color -> Gl.one_minus_dst_color
  | `Src_alpha -> Gl.src_alpha
  | `One_minus_src_alpha -> Gl.one_minus_src_alpha
  | `Dst_alpha -> Gl.dst_alpha
  | `One_minus_dst_alpha -> Gl.one_minus_dst_alpha
  | `Src_alpha_saturate -> Gl.src_alpha_saturate

let set_color_blend_factors r ~src ~dst = 
  Gl.blend_func r.ft (blend_factor_to_enum src) (blend_factor_to_enum dst)

