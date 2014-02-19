(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

open Gg;;

(** Textures. *)

(** {2 Creating} *)

val pow_two : int -> bool
    (** [true] if the argument is a power of two. *)
val ceil_pow_two : int -> int
    (** The argument's nearest greater or equal power of two. *)

type pf = [
  | `A of [ `C8 | `C16 | `C16f | `C32f ] 
  | `D of [ `C16 | `C24 | `C32 ]
  | `I of [ `C8 | `C16 | `C16f | `C32f ]
  | `L of [ `C8 | `C16 | `C16f | `C32f ]
  | `LA of [ `C4 | `C8 | `C16 | `C16f | `C32f ]
  | `RGB of [ `C8 | `P332 | `P555 | `C16 | `C16f | `C32f | `DXT1 ]
  | `RGBA of [ `C2 | `C4 | `P5551 | `C8 | `P1010102 | `C16 | `C16f | `C32f | 
               `DXT1 | `DXT3 | `DXT5 ]]
(** Texture internal pixel format. *)

val nearest_pf : 'a Image.pf -> pf 

type cube_face = [ `Pos_x | `Neg_x | `Pos_y | `Neg_y | `Pos_z | `Neg_z ]
(** Cube map faces. *)

type mag_f = [ `Nearest | `Linear]
(** Magnification filters. *)

type min_f = [ mag_f | `Nearest_mipmap_nearest | 
                    `Nearest_mipmap_linear | `Linear_mipmap_nearest | 
		    `Linear_mipmap_linear ]
(** Mignification filters. *)

type wrap = [ `Clamp | `Clamp_to_border | `Clamp_to_edge | 
                   `Mirrored_repeat | `Repeat ]
(** Texture adressing modes. *)

type kind = [ `D1 | `D2 | `D2_rect | `D3 | `Cube_map ]
(** Texture kinds. *)

type 'a t constraint 'a = [< kind]
(** The type for textures. *)

val create : Rend.t -> ?priority:float -> ?min_f:min_f -> 
  ?mag_f:mag_f -> ?lod_bias:float -> ?max_level:int ->
  ?wrap_s:wrap -> ?wrap_t:wrap -> ?wrap_r:wrap -> 
  w:int -> ?h:int -> ?d:int -> pf -> ([< kind] as 'a) -> 'a t
(** 
   Creates a texture with the given pixel format (as a hint) and kind.
    Arguments irrelevant for a texture kind are ignored. 
   {ul
   {- [priority], defaults to 1.0.}
   {- [min_f], [mag_f], mignification and magnification filters, 
    respectively defaults to [`Nearest_mipmap_linear] and [`Linear].}
   {- [max_level], number of additional mipmap levels, defaults to 0. 
    Clamped to truncate(log{_ 2} max(width, height, depth)).}
   {- [wrap_s], [wrap_t], [wrap_r], wrap parameter for texture coordinates, 
    defaults to [`Repeat].}
    {- [w], [h], [d], texture width, height and depth in pixels, 
    [h] and [d] default to 1.}
   }
*)

val priority : 'a t -> float
val set_priority : 'a t -> float -> unit
val min_f : 'a t -> min_f
val set_min_f : 'a t -> min_f -> unit
val mag_f : 'a t -> mag_f
val set_mag_f : 'a t -> mag_f -> unit
val lod_bias : 'a t -> float
val set_lod_bias : 'a t -> float -> unit
val max_level : 'a t -> int
val wrap_s : 'a t -> wrap
val set_wrap_s : 'a t -> wrap -> unit
val wrap_t : 'a t -> wrap
val set_wrap_t : 'a t -> wrap -> unit
val wrap_r : 'a t -> wrap
val set_wrap_r : 'a t -> wrap -> unit
val width : 'a t -> int
val height : 'a t -> int
val depth : 'a t -> int
val pf : 'a t -> pf
(** {b Note.} This is the format as you requested not the gpu internal 
    format TODO change that. *)
val kind : 'a t -> 'a

val extent : 'a t -> v3
(** Width, height, depth. *)

val extent2 : 'a t -> v2
(** Width, height. *)

val resident : 'a t -> bool
(** [true] if the texture is in video memory. *)

val print : Format.formatter -> 'a t -> unit

(** {2 Reading and writing} *)

val mipmap_bounds : 'a t -> level:int -> int * int * int
(** Width, height and depth of the given mipmap level. *)

val compute_mipmaps : 'a t -> unit
(** Computes mipmaps in hardware using the base level (0) data. *)

val blit_from_image : 'a Image.t -> ?level:int -> ?face:cube_face -> 
?x:int -> ?y:int -> ?z:int -> 'b t -> unit
(** Blits the texture with the given image. 
   {ul
   {- [level], the mipmap level to update, defaults to 0, the base level.}
   {- [face], for cube map textures : the face of the cube to update, 
    defaults to [`Pos_x].}
   {- [x,y,z], lower-left point where texture should be updated, defaults to 
    [(0,0,0)].}}
   Raises [Invalid_argument] if the image dimension doesn't match the
    texture dimension or if the image bounds exceed the texture's ones. 
*)

val blit_to_image :  ?level:int -> ?face:cube_face -> 'a t -> 
  'b Image.t -> unit
(** Blits the image with the given texture.
    {ul
    {- [level], the mipmap level to read from, defaults to 0 the base level.}
    {- [face], for cube map textures, the face of the cube to read from, 
     defaults to [`Pos_x]}} 
    Raises [Invalid_argument] if the image dimension and bounds 
    doesn't match the texture's ones, or if a compressed image format
    is used with a non compressed texture (the converse is allowed). 
*)

(**/**)
val destroy : 'a t -> unit
val _gl_bind_texture : 'a t -> unit

