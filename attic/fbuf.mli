(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

(** Frame buffers.

    Framebuffers are destination of drawing operations. They are made
    of more than one buffer.
   TODO Left/right/back/front buffer management (?), e.g. [contents]. 

*)
type t
(** The type for frame buffers. *)

(*
val default : Renderer.t -> t
(** Default frame buffer (created when the GL context was setup). *)

val current : Renderer.t -> t 
(** Destination of drawing operations. *)

val set : Renderer.t -> t -> unit
(** Sets the destination of drawing operations to the given drawable. *)

val size : t -> int * int
(** Frame buffer width and height in pixels. *)

val set_size : t -> w:int -> h:int -> unit
(** Sets the drawable's width and height in pixels *)
*)
(*
val contents : ?format:Pixel.format -> t ->  Image.t
(** Contents of the drawable as an image of the given
format (defaults to RGBA8888). *)

*)


(** Frame to frame statistics. 

   To gather frame to frame statistics call
   {!Fbuf.Stats.update} whenever you start rendering 
   a new frame.
*)
module Stats : sig

(** {2 Collecting} 

    Times are in seconds.
*)


  val reset : t -> unit
  (** Resets the statistics. *)

  val update : t -> unit
     (** Updates the statistics. *)

  (** {2:figs Figures} *)
 
  val frame_hz : t -> float
  (** Frame to frame frequency. *)

  val max_frame_hz : t -> float
  (** Maximal sampled frequency. *)
  
  val min_frame_hz : t -> float
  (** Minimal sampled frequency. *)

  val frame_time : t -> float
  (** Last frame to frame time. *)

  val max_frame_time : t -> float 
  (** Maximal frame to frame time. *)
  
  val min_frame_time : t -> float
  (** Minimal frame to frame time. *)
  
  val frame_vertex_count : t -> int 
  (** Last frame vertice count. *)
  
  val frame_face_count : t -> int 
  (** Last frame face count. *) 

(**/**)
  val add_geometry : t -> vertices:int -> faces:int -> unit
end
