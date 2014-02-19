(*---------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ---------------------------------------------------------------------------*)

(** Data buffers. 

    Data buffers of scalars.
*)   

(** {2 Creating} *)

type aim = [
   | `Attr (** Buffer holding vertex attribute data. *) 
   | `Index (** Buffer holding index data. *)
   | `Pixel_read (** Buffer from which the gpu will read pixels. *)
   | `Pixel_write (** Buffer to which the gpu will write pixels. *) ]
(** Purpose of the data, this is a hint (pixel read and write are 
    to be understood from the gpu's point of view). *)

type usage = [`Stream_draw | `Stream_read | `Stream_copy 
               | `Static_draw | `Static_read | `Static_copy 
	       | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]
(** Buffer usage patterns, this is an important {{:http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_buffer_object.txt} hint}. 
  The first part of the name identifies the data access pattern, 
    {e stream} - specify data once and use once or a few times, 
    {e static} - specify once and use many times, 
    {e dynamic} - specifiy and use many times. 
  The second part of the name distinguishes the data path, 
    {e draw} - application to OpenGL,
    {e read} - OpenGL to application, 
    {e copy} - OpenGL to OpenGL.
*)

type kind = [`Int8 | `Uint8 | `Int16 | `Uint16 | `Int32 | `Uint32 | 
             `Float32 | `Float64 ]
(** Buffer element kinds. *)

type float_kind = [ `Float32 | `Float64 ] 
type int_kind = [ `Int16 | `Int32 | `Int8 ] 
type uint_kind = [ `Uint16 | `Uint32 | `Uint8 ] 

(* val kind_bytes : kind -> int
(** Number of bytes for an element of the given kind. *)
*)

type +'a t constraint 'a = [< kind ]
(** The type for buffers. *)

val create : Rend.t -> ?aim:aim -> ?usage:usage -> ([< kind] as 'a) -> 
  int -> 'a t
(** Creates a buffer of the given kind and length. 
   {ul
   {- [aim], purpose of the buffer, this is a hint, defaults to 
      [`Attr].}
   {- [usage], how the buffer will be used, defaults to [`Static_draw].}} *)

val usage : 'a t -> usage
val kind : 'a t -> 'a
val length : 'a t -> int

val cast : ([< kind] as 'a) -> 'b t -> 'a t 
(** Specializes the buffer's type variable to the given kind. Raises
   [Invalid_argument] if the given kind is not equal to
   {!Buf.kind}. *)

val print : Format.formatter -> 'a t -> unit
(** Prints the buffer's information. *)

val print_data : Format.formatter -> ?pos:int -> ?len:int -> 'a t -> unit
(** Prints the buffer's element [pos] to [pos+len-1].  [pos] defaults to
   [0] and [len] to the maximal length possible.

   {b Warning.} This function {{:#map} maps} the buffer. *)

(** {2:rw Reading and writing} *)

type ('a, 'b) barray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

(** The following type and values are used to interface with {!Buf.barray}. *)

type ('a, 'b, 'c) array_kind 
val int8 : ([ `Int8 ], int, Bigarray.int8_signed_elt) array_kind
val uint8 : ([ `Uint8 ], int, Bigarray.int8_unsigned_elt) array_kind
val int16 : ([ `Int16 ], int, Bigarray.int16_signed_elt) array_kind
val uint16 : ([ `Uint16 ], int, Bigarray.int16_unsigned_elt) array_kind
val int32 : ([ `Int32 ], int32, Bigarray.int32_elt) array_kind
val uint32 : ([ `Uint32 ], int32, Bigarray.int32_elt) array_kind
val float32 : ([ `Float32 ], float, Bigarray.float32_elt) array_kind
val float64 : ([ `Float64 ], float, Bigarray.float64_elt) array_kind

(** {3:map Mapping (direct access)} 

   Mapping a buffer provides a {e transient}, direct access to its data in 
   a user provided callback.

   {b Warning.} Keep in mind the following points when mapping buffers. 
   - {b Never} try to escape the array or
   a subarray from the callback.  The bigarray given to the callback function is
   valid only during the callback call. 
   - {b Always} respect the {{:#TYPEaccess} access} policy that you specifiy 
   otherwise the operation may lead to program termination (see the 
   {{:http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_buffer_object.t
xt} spec}).
   - It is an error to remap the {e same} buffer in the callback.
   - It is an error to use the buffer to render while it is mapped. 
   - It is an error to blit to the buffer while it is mapped.
   - It is {b not} a problem if your callback raises an exception. *)

exception Corruption
(** *)


type access = [
  | `Read  (** Callback will only read data. *)
  | `Write (** Callback will only write data. *)
  | `Read_write (** Callback will read and write data. *) 
  | `Discard (** Callback will write the {e whole} buffer without reading. *)
]
(** Access policy. [`Discard] is write only. *)
val map : ('a, 'b, 'c) array_kind -> access -> (('b, 'c) barray -> unit) -> 
  'a t -> unit
(** Raises {!Buf.Corruption} before or after callback execution
   if the gpu corrupted the data. On some platforms this can
   occur if a screen resolution change happens at map time, see the
   {{:http://oss.sgi.com/projects/ogl-sample/registry/ARB/vertex_buffer_object.txt}
   spec}. Raises [Out_of_memory] if the buffer could not be
   mapped. In that case either the same buffer is mapped more
   than once or the buffer couldn't be allocated in the first
   place. *)

(** {3:blit Blitting} *)

val blit : src:'a t -> src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit
(** Blits between two buffers. If [src] physically equals [dest],
    doesn't handle overlapping data correctly.  

    {b Warning.} This function {{:#map} maps} both buffers.
*)

val blit_from_bigarray : ('a, 'b, 'c) array_kind -> src:('b, 'c) barray -> 
  src_pos:int -> dst:'a t -> dst_pos:int -> len:int -> unit   

val blit_to_bigarray : ('a, 'b, 'c) array_kind -> src:'a t -> src_pos:int -> 
  dst:('b, 'c) barray -> dst_pos:int -> len:int -> unit   

(**/**)

val _kind_bytes : [< kind ] -> int
val _gl_storage : [< kind ] t -> Gl.Enum.astorage
val _gl_bind_attribute : [< kind ] t -> int -> Gl.Ptr.c
val _gl_bind_index : [< kind ] t -> int -> Gl.Ptr.c
val _gl_bind_pixel_pack : [< kind ] t -> int -> Gl.Ptr.c
val _gl_bind_pixel_unpack : [< kind ] t -> int -> Gl.Ptr.c
val _ft : [< kind ] t -> Gl.Ptr.ft

val destroy : 'a t -> unit
(** Unclear whether we want that or not *)
