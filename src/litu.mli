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
    Buffers are created with {!Lit.Buf.create}'s default arguments. *)  
module Prim : sig

  (** {1 Primitives} 

      {b Note.} All these primitives are [`Triangles] primitives. *)

  val rect : ?name:string -> ?tex:string -> ?segs:Size2.t -> ?d2:bool ->
    [ `Size of Size2.t | `Box of box2 ] -> prim
  (** [rect ?name ?tex ?d2 segs spec] is an axis-aligned Oxy 2D plane 
      specified according to [spec]:
      {ul 
      {- [`Size s], the plane has extents [s] and is centered
         on the origin.}
      {- [`Box b], the plane is bounded by [b].}}

      The plane is divided in [Size2.w segs] segments along the x-axis
      and [Size2.h segs] along the y-axis ([segs] defaults to
      {!Size2.unit}, its number are rounded to integers). Each segment
      is made of two triangles.

      If [d2] is [true] (defaults to [false]), the primitive's vertex
      attribute has dimension 2.

      If [tex] is specified, 2D texture coordinates are added to 
      the primitive under that attribute name. The bottom left corner
      of the plane is (0,0) and top right (1,1). *)
 
  val cuboid : ?name:string -> ?dups:bool -> 
    [ `Size of Gg.size3 | `Box of box3 ] -> prim
  (** [cuboid dups spec] is an axis-aligned cuboid specified according to
      [spec]:
      {ul 
      {- [`Size s], the cuboid has extents [s] and is centered
         on the origin.}
      {- [`Box b], the cuboid is bounded by [b].}}
      If [dups] is [true] (default) vertices in the mesh are
      triplicated so that per vertex normal computation defines planar
      facets. *)

  val cube : ?name:string -> ?dups:bool -> float -> prim
  (** [cube name dups s] is [cube name dups (`Size (Size3.v s s s))]. *)

  val sphere : ?name:string -> ?level:int -> float -> prim
  (** [sphere level r] is a sphere of radius [r] centered 
      on the origin obtained by subdividing an octahedron ([level = 0]). 
      [level] defaults to [4]. The number of vertices is 4{^level + 1} + 2 and
      the number of triangles is 8 * 4{^ level}. If the number of vertices
      is greater than 2{^ 31} all sorts of bad things may happen... *) 

  (** {1 Functions} *) 

  val with_normals : ?scalar_type:[`Float32 | `Float64] -> 
    ?name:string -> prim -> prim 
  (** [with_normals prim] is [prim] with a {!Lit.Attr.normal}
      attribute added or replaced. The attribute has a normal per
      {!Lit.Attr.vertex} attribute computed from the primitive's
      triangles. In case a vertex belongs to multiple triangles 
      the contribution of each triangle is summed up and the result
      normalized.

      {b Warning.} Vertex data is looked up by taking vertices 
      from their buffer taking into account {!Lit.Attr.first} 
      and {!Lit.Attr.stride} {b until the end} of the buffer.

      @raise Invalid_argument if one of these conditions holds:
      {ul 
      {- [Prim.kind prim] is not [`Triangles]}
      {- There is no {!Lit.Attr.vertex} attribute or its dimension is not 3.}
      {- The {!Lit.Attr.vertex} attribute buffer is not in CPU memory or
         its bigarray buffer can't be read using [float]s.}
      {- The [Prim.index prim] buffer (if any) is not in CPU memory or 
         its bigarray buffer can't be read using [int]s or [int32].}} *)

end

(** Useful effects. *) 
module Effect : sig

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
        
    val viewport_size : Size2.t uniform 
    (** TODO *) 

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
    
    val create : ?raster:Effect.raster -> ?depth:Effect.depth -> 
      ?fill_color:Color.t -> ?wire_color:Color.t -> 
      ?wire_width:float -> ?wire_only:bool -> unit -> Effect.t
    (** [create ()] is a new wireframe effect, the optional parameters
        can be used to preset the uniforms. *)
  end
end

(** 3D space manipulators *) 
module Manip : sig

  type rot 
  (** The type for rotation manipulators. *) 

  val rot : ?center:p2 -> ?radius:float -> ?init:quat -> start:p2 -> unit -> 
    rot    
  val rot_update : rot -> p2 -> quat 
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
