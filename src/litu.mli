(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Lit utilities 

    Convenient utilities for working with Lit. Higher-level
    {{!Prim}primitives} (cuboids, spheres, etc.). Predefined 
    effects (wireframe, flat and phong shading). Space navigation. *) 

open Gg
open Lit

(** {1 Primitives} *) 

(** Higher level primitives. 

    All these primitives define at least a {!Lit.Attr.vertex} attribute. 
    Buffers are created with {!Buf.create}'s default arguments. *)  
module Prim : sig

  (** {1 Primitives} 

      {b Note.} All these primitives are [`Triangles] primitives. *)
 
  val cuboid : ?name:string -> ?dups:bool -> Gg.size3 -> prim
  (** [cuboid dups extents] is a cuboid centered on the origin with
      extents [extents]. If [dups] is [true] (default) vertices in the
      mesh are triplicated so that per vertex normal computation defines 
      planar facets. *)

  val cube : ?name:string -> ?dups:bool -> float -> prim
  (** [cube name dups s] is [cube name dups (Size3.v s s s)]. *)

  val sphere : ?name:string -> ?level:int -> float -> prim
  (** [sphere level r] is a sphere of radius [r] centered 
      on the origin obtained by subdividing an octahedron ([level = 0]). 
      [level] defaults to [4]. The number of vertices is 4{^level + 1} + 2 and
      the number of triangles is 8 * 4{^ level}. If the number of vertices
      is greater than 2{^ 31} all sorts of bad things may happen... *) 

  val rect : ?name:string -> ?tex:string -> ?segs:Size2.t -> Size2.t -> prim
  (** [rect ?name ?tex segs size] is an Oxy plane of width [Size2.w
      size] and height [Size2.h size] centered on the origin.  

      The plane is divided in [Size2.w segs] segments along the x-axis
      and [Size2.h] along the y-axis ([segs] defaults to
      {!Size2.unit}, its number is rounded to integers). Each segment
      is made of two triangles.

      If [tex] is specified, 2D texture coordinates are added to 
      the primitive under that attribute name. Bottom left is
      (0,0), top right (1,1). *)
end

(** Higher level effects. *) 
module Effects : sig

  (** Triangles as wireframes.

      This effect renders [`Triangle] primitives as 
      wireframes.
   
      {b References.}  
      {ul 
      {- J.A Boerentzen et al. 
         {{:http://cgg-journal.com/2008-2/06/index.html.}
         Shader-Based Wireframe Drawing}. 
    Computer Graphics & Geometry Journal, 2008.}}. *)
  module Wireframe : sig 
    
    (** {1 Effect} *) 

    val model_to_clip : M4.t uniform 
    (** [model_to_clip] is the model to clip space transform matrix, 
        defaults to the [`Model_to_clip] built-in. *)

    val fill_color : Color.t uniform 
    (** [fill_color] is the triangle's fill color, default to 
        {!Color.white}. *) 

    val wire_color : Color.t uniform 
    (** [wire_color] is the wireframe color, defaults to {!Color.black}. *)

    val wire_width : float uniform 
    (** [wire_width] is the wire width (TODO in which space), default to 
        [1]. *)

    val wire_only : bool uniform 
    (** [wire_only], if [true] only the wireframe is shown, defaults to 
        [false]. *) 
    
    val create : ?fill_color:Color.t -> ?wire_color:Color.t -> 
      ?wire_width:float -> ?wire_only:bool -> unit -> Effect.t
    (** [create ()] is a new wireframe effect, the optional parameters
        can be used to preset the uniforms. *)
  end
end

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
