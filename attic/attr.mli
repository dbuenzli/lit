(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

(** Vertex attribute buffers (F). 

   Vertex attribute buffers hold per vertex information in a buffer. An 
   attribute can be a vector or a matrix specified in column order. *)

(** {2 Format} *)

type kind = [ `V1 | `V2 | `V3 | `V4 | `M2 | `M3 | `M4 ]
(** Attribute kinds. *)

val kind_size : [< kind] -> int
(** Kind's number of components. *)

type 'a format constraint 'a = [< kind]
(** The type for attribute buffer formats. *)

val format : ?normalized:bool -> ?first:int -> ?skip:int ->  
   ([< kind] as 'a) -> 'a format
(** Creates a format. 
   {ul 
   {- [normalized], if [true] fixed point types are normalized when they are 
    converted to floating points by the gpu. Maps unsigned values to [[0;1]] 
    and signed ones to [[-1;-1]]. Defaults to [false].}
   {- [first], buffer element where the data
   starts, defaults to [0].}
   {- [skip], number of buffer elements to skip between 
   two consecutive attributes, defaults to [0].}} *)

val normalized : 'a format -> bool
val first : 'a format -> int
val skip : 'a format -> int
val kind : 'a format -> 'a
val print_format : Format.formatter -> 'a format -> unit

(** {2 Attribute} *)

type ('a, 'b) t = 'a Buf.t * 'b format 
(** The type for attribute buffers. *)

val pos : ('a, 'b) t -> int -> int 
(** The attribute's starting position in the buffer. *)

val size : ('a, 'b) t -> int 
(** Buffer elements per attribute (see {!Attr.kind_size}). *)

val max : ('a, 'b) t -> int
(** Maximum number of attributes the buffer can hold. *)

val print : Format.formatter -> ?pos:int -> ?len:int -> ('a, 'b) t -> unit
(** Prints {e attributes} [pos] to [pos+len-1]. 
   [pos] defaults to [0] and [len] to the maximal length possible. *)

(**/**)

val _gl_source : Gl.Enum.client_cap -> ('a, 'b) t -> unit
val _gl_unsource : Gl.Ptr.ft -> Gl.Enum.client_cap -> unit
