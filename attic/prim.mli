(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

(** Rendering primitives. *)

(** Specifies how consecutive vertex attributes should be spatially 
   interpreted. *)
type kind = [
   | `Points      (** List of points, one vertex per point. *)
   | `Lines       (** List of lines, two vertice per line. *)
   | `Line_loop   (** Same as line strip, but v{_max} is connected to v{_0}.*)
   | `Line_strip  (** Connected lines, (v{_n},v{_n+1}) defines line n+1. *)
   | `Triangles   (** Triangle list. *)
   | `Triangle_strip (** Connected triangles. *)
   | `Triangle_fan    (** Linked fan of triangles. *)
   | `Quads          (** List of quads. *)
   | `Quad_strip (** Connected quads. *)
]
