(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

(** Vertex index buffers (F). 

   Index buffers are used to index the attributes of vertex attribute buffers. 
*)

(** {2 Format} *)

type format 
(** The type for index buffer formats. *)

val format : ?range:int*int -> ?first:int -> unit -> format
(** Creates a format. 
   {ul 
   {- [range:(min,max)], when given, all values in the index must lie in
   [[min;max]]. It is an error (with platform dependent behaviour) 
   for indices to lie outside that range. To get maximal performance
   it is recommended that the length of your index is not greater than 
   {!Rend.limits}[.max_elements_indices] and
   that [max-min+1] is not greater {!Rend.limits}[.max_elements_vertices].}
   {- [first] buffer element where the index data
   starts, defaults to [0].}} *)
   
val range : format -> (int * int) option
val first : format -> int
val print_format : Format.formatter -> format -> unit

(** {2 Index} *)

type 'a t = 'a Buf.t * format constraint 'a = [< Buf.uint_kind ]
(** The type for vertex index buffers. *)

val pos : 'a t -> int -> int
(** Position of the given indice in the buffer. *)

val max : 'a t -> int 
(** Maximum number of indices the buffer can hold. *)

val print : Format.formatter -> ?pos:int -> ?len:int -> 'a t -> unit
(** Prints indices [pos] to [pos+len-1]. [pos] defaults to [0] and 
    [len] to the maximal length possible. *) 

(**/**)
val _gl_draw : Gl.Enum.mode -> count:int -> 'a t -> unit
