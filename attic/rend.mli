(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

(** Interface to the renderer. 

    A renderer must be created after a valid OpenGL context was setup.
    In our terminology, a renderer corresponds to an OpenGL context.
*)

open Gg;;
open Bigarray;;


type t
 
val create : unit -> t
(** Creates a renderer. Raises {!Rend.Unsupported} if the context
    doesn't support at least OpenGL 1.3 and the
    {{:../lit003.html#toc4} required} extensions. *)

(** {2 Information} *)

val print_info : Format.formatter -> t -> unit
(** Prints the renderer's opengl info, caps and limits. *)

type opengl_info = 
    { vendor : string; 
      (** vendor string. *)
      renderer : string; 
      (** renderer string. *)
      version : string * (int * int); 
      (** version string and extracted number *)
      shading_language_version : string * (int * int); 
      (** shading language version string and extracted number. *)
      extensions : string list 
      (** extensions (sorted alphabetically) *) }
      (** OpenGL renderer information. *) 

val opengl_info : t -> opengl_info    
val print_opengl_info : Format.formatter -> opengl_info -> unit
      
type caps = 
    { dxt_textures : bool;
      (** True if dxt (s3tc) compressed texture formats are supported. *)
      npot_textures : bool; 
      (** True if non power of two textures are supported. *)
      rect_textures : bool;
      (** True if limited rectangular textures are supported. *) 
      float_textures : bool; 
      (** True if textures with floating point formats are supported. *) }
      (** OpenGL optional capabilities. *)

val caps : t -> caps
val print_caps : Format.formatter -> caps -> unit

(** The following functions check capabilities directly. Use to
conditionalize your code. *)

val has_dxt_textures : t -> bool
val has_npot_textures : t -> bool
val has_rect_textures : t -> bool
val has_float_textures : t -> bool

(**/**)
val sdxt_textures : t -> unit
val snpot_textures : t -> unit
val srect_textures : t -> unit
val sfloat_textures : t -> unit
(**/**)


type limits = 
    { max_elements_indices : int;  
      (** Recommended maximal number of indices in an index buffer with 
	  range. *)    
      max_elements_vertices : int;     
      (** Recommended maximal number of different vertices indexed by an index
	  buffer with range. *)
      max_clip_planes : int;	
      (** Maximum number of clip planes. *)       
      max_texture_size : int;	       
      (** Maximum width or height of a texture image. *)
      max_3d_texture_size : int;
      (** Maximum width of height or depth of a 3D texture image. *) 
      max_fragment_uniform_components : int;
      (** Maximum number of floating point values for uniform variables  
       in the fragment shader. *)
      max_fragment_varying_components : int;
      (** Maximum number of floating point values for varying 
	  variables in the fragment shader. *)
      max_fragment_texture_image_units : int;
      (** Maximum number of texture units that can be used by a 
	  fragment shader. *)
      max_vertex_attributes : int;
      (** Maximum number of vertex attributes in a vertex shader. *)
      max_vertex_uniform_components : int;
      (** Maximum number of floating point values for uniform variables 
       in the vertex shader. *)
      max_vertex_texture_image_units : int;
      (** Maximum number of texture units that can be used by 
	  a vertex shader. *)
      max_combined_texture_image_units : int;
      (** Maximum number of texture units that can be used by
	  the vertex and the fragment shader togheter. *) }
      (** OpenGL renderer limits. Integer values may not be completly
   accurate because of OCaml's [int]s. *) 

val limits : t -> limits 
val print_limits : Format.formatter -> limits -> unit


(** {2 Errors} *)

exception Unsupported of opengl_info * string list
(** Raised if the renderer cannot be used with the library. Lists
    the missing required extensions. *)



exception Error of string
(** Raised when unsupported features are used (see {!Rend.caps}) or when
 an OpenGL error occurs (see {!Rend.error}). *) 

val error : t -> ('a -> 'b) -> 'a -> 'b  
  (** Calls the function with the given argument and raises 
      {!Rend.Error} if an OpenGL error occurs. *) 


(** {2:framebuf Framebuffer selection and clearing} *)

val flush : ?block:bool -> t -> unit
(** Forces execution of the issued rendering commands. If
   block is [true] (defaults to [false]) the function returns only
   when all pending commands have been executed and the framebuffer updated.

   {b Note.} This function should not be called with double buffered
    contexts since this is managed in their own buffer swapping
    function.  *)


(** Type specifiying a buffer clearing value. *)
type clear_buf = [
   | `Color of v4       (** RGBA *) 
   | `Depth of float 
   | `Stencil of int32
   | `Accum of v4 ]

val clear_fbuf : t -> clear_buf list -> unit
(** Clears buffers with specified values. 

   {b Note.} Clearing is clipped to the {{:#clip} clip rectangle} and
   is  affected by the individual buffer's write settings (see 
   {!Rend.enable_color_write}, {!Rend.enable_depth_write} and
   {!Rend.enable_stencil_write}). *)

(*

(** Type specifying a color buffer. *)
type color_buffer =                             
  | Nil
  | Front_left
  | Front_right
  | Back_left
  | Back_right
  | Front
  | Back
  | Left
  | Right
  | Front_and_back
  | Aux of int
	

val set_color_buffer : t -> color_buffer -> unit
(** Sets the destination of color buffer updates. Initially set to
   [Front] for single buffered contexts and [Back] for double buffered
   ones. *)
*)


(** {2:matr Geometry stage} *)

val set_modelview_matrix : t -> ?mult:bool -> m4 -> unit
(** Sets the modelview matrix. If mult is [true], the matrix multiplies 
   the current matrix on the right. Initially set to the identity matrix. 

   Initially the viewer is looking along the negative z-axis, with the y-axis
   pointing up and the x-axis to the right. 
*)

val set_projection_matrix : t -> ?mult:bool -> m4 -> unit
(** Sets the projection matrix. If mult is [true], the matrix multiplies
   the current matrix on the right. Initially set to the identity matrix. *)

val set_texture_matrix : t -> ?mult:bool -> unit:int -> m4 -> unit
(** Sets the texture matrix of the given texture [unit] (which should
   be smaller than. If mult is
   [true], the matrix multiplies the current matrix on the
   right. Initially set to the identity matrix.*)

(** {2:win Screen coordinates mapping and clipping} *)

val set_viewport : t -> o:v2 -> size:v2 -> unit
(** Sets the viewport rectangle by specifying its lower left corner
   and size in screen coordinates (floats are truncated). This sets
   the affine transform from normalized device coordinates [nd] to
   screen coordinates ([o]{_ x} + ([nd]{_ x}*[size]{_ x}), [o]{_ y} +
   ([nd]{_ y}*[size]{_ y})).

   {b Note.} The viewport doesn't clip fragments. Use the clip rectangle
   for that. 
*)

val set_depth_range : t -> near:float -> far:float -> unit 
(** Sets the linear mapping from normalized device {e depth}
   coordinates (lying in \[-1;1\]) to screen depth coordinates (in
   \[near;far\] clamped to \[0;1\]).  Initially \[0;1\].

   {b Note.} Reverse mapping (with [near > far]) are allowed. *)

val set_clip_rect : t -> o:v2 -> size:v2 -> unit
(** Sets a clip rectangle by specifying its lower left corner and
   size in screen coordinates. Whenever clipping is 
   {{:#VALenable_clip_rect} enabled}, fragments lying strictly outside the
   rectangle are not rendered. *)

val enable_clip_rect : t -> bool -> unit
(** Enables fragment clipping to the rectangle specified with
{!Rend.set_clip_rect}. Initially disabled. *)


(** {2:facecull Face culling} *)

(** Type specifying face culling modes. *)
type face_cull = [
  | `None               (** Disable face culling *)
  | `Clockwise          (** Cull faces whose vertices in screen coordinates
			       are ordered clockwise. *)
  | `Counter_clockwise  (** Cull faces whose vertices in screen coordinates
			       are ordered counter clockwise. *)]

val set_face_cull : t -> face_cull -> unit
(** Sets the face culling mode. Initially [`None]. *)


(** {2:rast Rasterization} *)

(** Type specifying shading modes. *)
type shading = [ 
  | `Flat   (** Shading is computed per face. *)
  | `Smooth (** Shading is computed per vertex and 
			  interpolated accross the face. *) ]

val set_shading : t -> shading -> unit
(** Sets the shading mode. Initially [`Smooth]. *)

(** Type specifying rasterization modes for faces. *)
type raster = [
  | `Vertices (** Faces' specification vertices are drawn. *)
  | `Lines    (** Faces' line boundaries are drawn. *)
  | `Faces    (** Faces are drawn. *) ]

val set_raster : t -> raster -> unit
(** Sets the rasterization mode. Initially [Raster_faces]. *)

val set_depth_offset : t -> float -> unit 
(** Offsets the depth value of a primitive to avoid depth fighting
  (e.g. for decals). Initially 0.0, typically use 1.0 to push the
  primitive back. *)


(** {2:framebuf Fragment operations} *)

(** Type specifying comparison functions. *)
type compare_fun = [
  | `False
  | `Lt 
  | `Eq 
  | `Leq
  | `Gt
  | `Neq 
  | `Geq 
  | `True ]


(** {3:alphatest Alpha test} *)

val enable_alpha_test : t -> bool -> unit
(** Enables alpha test of incoming fragment's alpha values
   against a constant reference value. Fragments for which the alpha
   test fails are discarded. See {!Rend.set_alpha_test}. *)

val set_alpha_test : t -> compare_fun -> ref:float -> unit
(** When alpha testing is enabled, specifies the comparison function
 and the reference value (clamped to \[0;1\]) against which incoming
 alpha values are compared. The test succeeds when [incoming compare_fun
 ref] is [true]. Initially [Always].*)


(** {3:stenciltest Stencil buffer and test} *)

val enable_stencil_test : t -> bool -> unit
(** Enables stencil test and stencil buffer updates. Fragments
   for which the stencil test fails are discarded. Initially
   disabled. *)

val enable_stencil_write : t -> int32 -> unit
(** When the stencil test is enabled, enables stencil buffer writing on
   the given bits set to one. Initially set to [0xFFFFFFFFl]. *)

val set_stencil_test : t -> compare_fun -> ref:int32 -> mask:int32 -> unit
(** When the stencil test is enabled, specifies the comparison
   function and the reference value and mask against which stencil
   values are compared. The test succeeds when [(ref && mask)
   compare_fun (stencil && mask)] is [true]. Initially [Always]. *)

(** Type specifing how the stencil buffer is updated. *)
type stencil_update = [
  | `Zero     (** Sets the value to 0. *)
  | `Keep     (** Keeps the current value. *)
  | `Replace  (** Sets the value to the current [ref] value (see 
		 {!Rend.set_stencil_test}) *)
  | `Incr     (** Increments the current value. Clamped to max. *)
  | `Decr     (** Decrements the current value. Clamps to 0. *)
  | `Invert   (** Bitwise inversion of the current value. *)]
      
val set_stencil_update : t -> stencil_fail:stencil_update -> 
   depth_fail:stencil_update -> depth_pass:stencil_update -> unit
(** When the stencil test is enabled, specifies how the stencil is
   updated when a fragment passes or fails the stencil and the depth
   test (the depth test occurs after the stencil test).  Initially
   [Keep], [Keep], [Keep]. *)

(** {3:depthbuf Depth buffer and test} *)

val enable_depth_test : t -> bool -> unit
(** Enables depth test and depth buffer updates. Fragments for which
   the depth test fails are discarded. Initially disabled.*)

val enable_depth_write : t -> bool -> unit
(** When the depth test is enabled, enables depth buffer
   writing. Initially enabled. *)

val set_depth_test : t -> compare_fun -> unit
(** When the depth test is enabled, specifies the function
   used to compare incoming depth values with the depth value
   in the depth buffer. The test succeeds when [incoming compare_fun buffer]
   is [true]. Initially [Less]. *)


(** {3:colorbuf Color buffer} *)

val enable_color_write : t -> r:bool -> g:bool -> b:bool -> a:bool -> unit 
(** When a color buffer is set, enables writing on specific color
   components. Initially enabled for all components. *)

val enable_color_blend : t -> bool -> unit
(** Enables blending between incoming fragments (R{_s} G{_s} B{_s}
   A{_s}) and values (R{_d} G{_d} B{_d} A{_d}) stored in the color
   buffer. When enabled the final value written in the color buffer is
   ((R{_s}S{_r} + R{_d}D{_r}) (G{_s}S{_g} + G{_d}D{_g}) (B{_s}S{_b} +
   B{_d}D{_b}) (A{_s}S{_a} + A{_d}D{_a})) where (S{_r} S{_g} S{_b}
   S{_a}) and (D{_r} D{_g} D{_b} D{_a}) are the source and destination
   blending factors.*)

(** Types to specify blend factors. *)

type dst_blend_factor = [ 
  | `Zero (** (0 0 0 0) *)
  | `One (** (1 1 1 1) *)
  | `Src_color(** (R{_s} G{_s} B{_s} A{_s}) *)
  | `One_minus_src_color(** (1 1 1 1) - (R{_s} G{_s} B{_s} A{_s}) *) 
  | `Dst_color (** (R{_d} G{_d} B{_d} A{_d}) *)	     
  | `One_minus_dst_color(** (1 1 1 1) - (R{_d} G{_d} B{_d} A{_d}) *)
  | `Src_alpha(** (A{_s} A{_s} A{_s} A{_s}) *)	     
  | `One_minus_src_alpha(** (1 1 1 1) - (A{_s} A{_s} A{_s} A{_s}) *)
  | `Dst_alpha(** (A{_d} A{_d} A{_d} A{_d}) *)	     
  | `One_minus_dst_alpha(** (1 1 1 1) - (A{_d} A{_d} A{_d} A{_d}) *)]
type src_blend_factor = [ dst_blend_factor | 
`Src_alpha_saturate(** (f f f 1) with f = min A{_s} (1 - A{_d}) *)]

val set_color_blend_factors : t -> src:src_blend_factor -> dst:dst_blend_factor -> unit 
(** When blending is enabled specifies the source (S{_r} S{_g} S{_b}
 S{_a}) and destination (D{_r} D{_g} D{_b} D{_a}) blend factors. *)





(*
val enable_multisample : t -> bool -> unit
val multisample_coverage : ~clamp:float -> invert:bool -> unit
val set_clip_planes : plane list -> unit 
val enable_clip_plane : int -> bool -> unit
*)
(**/**)

val _ft : t -> Gl.Ptr.ft
