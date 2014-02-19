(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

(** Image buffers (F). 

Image buffers store pixel information in a buffer. Images can have
from one to three dimensions. The first pixel of the buffer 
is the image's lower left frontmost pixel. *)

open Gg;;

(** {2 Format} *)

type dim = [ `D1 | `D2 | `D3 ]
(** Image dimension. *)

type comps = [ `A | `R | `G | `B | `RGB | `ARGB | `RGBA | `LA | `L | `D ]
(** Pixel components. *)

val comps_count : comps -> int
(** Number of components. *)

type storage = [ Buf.uint_kind | `Float32]

type 'a pf = [ 
  | `A of 'a
  | `R of 'a
  | `G of 'a 
  | `B of 'a
  | `L of 'a
  | `LA of 'a
  | `RGB of 'a * [`C | `P332 | `P565 | `DXT1 ]
  | `RGBA of 'a * [`C | `P4444 | `P5551 | `P8888 | `P1010102 | 
                   `DXT1 | `DXT3 | `DXT5]
  | `ARGB of 'a * [`P4444 | `P1555 | `P8888 | `P2101010 ]]
constraint 'a = [< storage ]
(** Pixel formats. 

    Pixel formats [`A], [`R], [`G], [`B], [`L], [`LA], [`RGB] and 
    [`RGBA] with [`C] store each component in a single
    buffer element with the leftmost component first. For example
    [`RGB (`Float32, `C)] stores each pixel in a sequence of three
    [`Float32] elements, red first. [`C_rev] 

    Pixel formats [`RGB], [`RGBA] and [`ARGB] with explicit bit
    layouts store the whole pixel in a single buffer element. The
    leftmost component is stored in the most significant bit. The
    storage kind must be an integer whose size fits the bits.  
    For example [`RGB (`Uint8, `P332)] stores a pixel in a 
    byte as [0bRRRGGGBB].

    Pixel formats [`RGB] and [`RGBA] with [`DXT1], [`DXT2] and
    [`DXT3] store pixel data according to the
    {{:http://en.wikipedia.org/wiki/S3_Texture_Compression}S3TC
    format}. The storage kind must be equal to [`Uint8].

    For fast integer and floating point data transfers
    between the cpu and the gpu, [`ARGB (`Uint32, `P8888)] and 
    [`RGBA (`Float32, `C)] seem to be good multiplatform choices. *)

val pf_compressed : 'a pf -> bool
(** True if the pixel format is compressed. *)

val pf_storage : 'a pf -> 'a
(** Storage of the pixel format. *)

val pf_cast : ([< storage] as 'a) -> 'b pf -> 'a pf
(** Specializes the pixel format's type variable. Raises [Invalid_argument]
    if the given storage is not equal to {!Image.storage}. *)


type 'a format constraint 'a = [< storage ]
(** The type for image formats. *)

val format : ?first:int -> ?w_skip:int -> ?h_skip:int -> w:int ->
  ?h:int -> ?d:int -> 'a pf -> dim -> 'a format
(** Creates a new format for an image with the given pixel format
    and dimension. Arguments irrelevant for a dimension are ignored. 
   {ul
   {- [first], buffer element where the data of the bottom, left, frontmost 
    pixel [(0,0,0)] starts.}
   {- [w_skip], number of {e pixels} to skip between two 
    consecutive lines, defaults to [0].} 
   {- [h_skip], number of {e lines} to skip between two 
    consecutive images, defaults to [0].}
   {- [w], [h], [d], image width, height and depth in pixels, 
    [h] and [d] default to [1].}
   }
  
    {b Note.} [Invalid_argument] is raised if the bit layout of 
    a pixel format doesn't fits its storage and for compressed pixel formats : 
    if the storage is not [`Uint8] or the skip values are different from 
    [0] or the dimension in not [`D2].  *)

val first : 'a format -> int
val width_skip : 'a format -> int
val height_skip : 'a format -> int
val width : 'a format -> int
val height : 'a format -> int
val depth : 'a format -> int
val pf : 'a format -> 'a pf
val dim : 'a format -> dim
val storage : 'a format -> 'a
val comps : 'a format -> comps

val extent : 'a format -> v3
(** Width, height, depth. *)

val extent2 : 'a format -> v2
(** Width, height. *)

val size : 'a format -> int
(** Number of buffer elements needed to hold the image. *)

val sub_format : 'a format -> ?x:int -> ?y:int -> ?z:int -> ?w:int -> 
  ?h:int -> ?d:int -> ?dim:dim -> unit -> 
    'a format  
(** Format for a subimage of the given format.
   {ul
   {- [x],[y],[z], new origin of the image in pixels, defaults to [0,0,0].}
   {- [w],[h],[d], new size of the image, defaults to the original size
    minus the new origin.}
   {- [dim], new dimension of the image, must be smaller than the original one,
    defaults to the original dimension.}}

  {b Note.} Raises [Invalid_argument] on compressed formats. 
*)

val print_format : Format.formatter -> 'a format -> unit

(** {2 Image} *)

type 'a t = 'a Buf.t * 'a format constraint 'a = [< Buf.uint_kind | `Float32 ] 
(** The type for image buffers. *)

val pos : 'a t -> x:int -> y:int -> int
(** Position of the given pixel in the buffer (assumes [z = 0]). 
    Irrelevant for compressed formats. 
*) 

val pos3 : 'a t -> x:int -> y:int -> z:int -> int
(** Position of the given pixel in the buffer. Irrelevant 
    for compressed formats. *)

val pixel_size : 'a t -> int 
(** Buffer elements per pixel. Irrelevant for compressed formats. *)

val sub : 'a t -> ?x:int -> ?y:int -> ?z:int -> ?w:int -> 
  ?h:int -> ?d:int -> ?dim:dim -> unit -> 'a t  
(** Applies {!Image.sub_format} to the image's format. *)

val cast : ([< storage] as 'a) -> 'b t -> 'a t 
(** Specializes the image's type variable. Raises [Invalid_argument]
    if the given storage is not equal to {!Image.storage}. *)

val print : Format.formatter -> ?x:int -> ?y:int -> ?z:int -> ?w:int -> ?h:int -> ?d:int -> 'a t -> unit
(** Prints the given pixel range,  higher lines first. *)

(**/**)
val _gl_set_pixel_pack : Gl.Ptr.ft -> 'a format -> unit
val _gl_set_pixel_unpack : Gl.Ptr.ft -> 'a format -> unit
val _gl_pformat : 'a format -> Gl.Enum.pformat
val _gl_pstorage : 'a format -> Gl.Enum.pstorage
val _string_of_dim : dim -> string

