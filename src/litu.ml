(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Lit

module Prim = struct

  let cuboid_dups ?name extents = 
    let x, y, z = V3.(to_tuple (0.5 * extents)) in
    let attrs = 
      let b = Ba.create Bigarray.float32 (8 * 3 * 3) in 
      let i = Ba.set_3d b 0 (-.x) (-.y) (  z) in (* Front *)
      let i = Ba.set_3d b i (  x) (-.y) (  z) in
      let i = Ba.set_3d b i (  x) (  y) (  z) in
      let i = Ba.set_3d b i (-.x) (  y) (  z) in
      let i = Ba.set_3d b i (-.x) (-.y) (  z) in (* Bottom *)
      let i = Ba.set_3d b i (  x) (-.y) (  z) in
      let i = Ba.set_3d b i (  x) (-.y) (-.z) in
      let i = Ba.set_3d b i (-.x) (-.y) (-.z) in
      let i = Ba.set_3d b i (-.x) (-.y) (  z) in (* Left *)
      let i = Ba.set_3d b i (-.x) (-.y) (-.z) in
      let i = Ba.set_3d b i (-.x) (  y) (-.z) in
      let i = Ba.set_3d b i (-.x) (  y) (  z) in
      let i = Ba.set_3d b i (  x) (-.y) (  z) in (* Right *)
      let i = Ba.set_3d b i (  x) (-.y) (-.z) in
      let i = Ba.set_3d b i (  x) (  y) (-.z) in
      let i = Ba.set_3d b i (  x) (  y) (  z) in
      let i = Ba.set_3d b i (  x) (  y) (  z) in (* Top *)
      let i = Ba.set_3d b i (  x) (  y) (-.z) in
      let i = Ba.set_3d b i (-.x) (  y) (-.z) in
      let i = Ba.set_3d b i (-.x) (  y) (  z) in
      let i = Ba.set_3d b i (-.x) (  y) (-.z) in (* Rear *)
      let i = Ba.set_3d b i (-.x) (-.y) (-.z) in
      let i = Ba.set_3d b i (  x) (-.y) (-.z) in
      let _ = Ba.set_3d b i (  x) (  y) (-.z) in
      [ Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray b)) ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (6 * 2 * 3) in 
      let i = Ba.set_3d b 0 4 7 6 in  (* Bottom *)
      let i = Ba.set_3d b i 4 6 5 in 
      let i = Ba.set_3d b i 8 1 0 in  (* Left *)
      let i = Ba.set_3d b i 8 0 9 in 
      let i = Ba.set_3d b i 12 4 5 in  (* Right *)
      let i = Ba.set_3d b i 12 3 4 in 
      let i = Ba.set_3d b i 16 7 8 in  (* Top *) 
      let i = Ba.set_3d b i 16 8 9 in 
      let i = Ba.set_3d b i 20 3 2 in  (* Rear *)
      let _ = Ba.set_3d b i 20 2 1 in 
      Buf.create (`Bigarray b)
    in
    Prim.create ?name ~index `Triangles attrs 

  let cuboid_no_dups ?name extents = 
    let x, y, z = V3.(to_tuple (0.5 * extents)) in
    let attrs = 
      let b = Ba.create Bigarray.float32 (3 * 8) in 
      let i = Ba.set_3d b 0 (-.x) (-.y) (  z) in 
      let i = Ba.set_3d b i (  x) (-.y) (  z) in 
      let i = Ba.set_3d b i (-.x) (  y) (  z) in 
      let i = Ba.set_3d b i (  x) (  y) (  z) in 
      let i = Ba.set_3d b i (-.x) (-.y) (-.z) in 
      let i = Ba.set_3d b i (  x) (-.y) (-.z) in 
      let i = Ba.set_3d b i (-.x) (  y) (-.z) in 
      let _ = Ba.set_3d b i (  x) (  y) (-.z) in
      [ Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray b)) ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (6 * 2 * 3) in
      let i = Ba.set_3d b 0 0 3 2 in
      let i = Ba.set_3d b i 0 1 3 in
      let i = Ba.set_3d b i 0 5 1 in
      let i = Ba.set_3d b i 0 4 5 in
      let i = Ba.set_3d b i 0 6 4 in
      let i = Ba.set_3d b i 0 2 6 in
      let i = Ba.set_3d b i 1 7 3 in
      let i = Ba.set_3d b i 1 5 7 in
      let i = Ba.set_3d b i 2 7 6 in
      let i = Ba.set_3d b i 2 3 7 in
      let i = Ba.set_3d b i 4 7 5 in
      let _ = Ba.set_3d b i 4 6 7 in
      Buf.create (`Bigarray b)
    in
    Prim.create ?name ~index `Triangles attrs 

  let cuboid ?name ?(dups = true) extents =
    if dups then cuboid_dups ?name extents else 
    cuboid_no_dups ?name extents 

  let cube ?name ?dups s = cuboid ?name ?dups (Size3.v s s s)

  (* Sphere *) 

  module Edge = struct
    type t = int * int                     (* vertex indices for the edges. *) 
    let compare (v0, v1 as v) (u0, u1 as u) =  (* insensitive to direction. *) 
      if (v0 = u0 && v1 = u1) || (v0 = u1 && v1 = u0) then 0 else
      compare v u       
  end 
  module Emap = Map.Make (Edge)

  let sphere ?name ?(level = 4) r =
    let ra = r /. sqrt 2. in 
    let four_pow n = 1 lsl (2 * n) in
    let vertex_count = 2 + four_pow (level + 1) in 
    let face_count = 8 * four_pow level in
    let vs = Ba.create Bigarray.float32 (vertex_count * 3) in 
    let is = Ba.create Bigarray.int32 (face_count * 3) in
    (* Level 0 isocahedron *)
    let i = Ba.set_3d vs 0 (  0.) (  0.) (  r) in     
    let i = Ba.set_3d vs i (  0.) (  0.) (-.r) in 
    let i = Ba.set_3d vs i (-.ra) (-.ra) ( 0.) in 
    let i = Ba.set_3d vs i (  ra) (-.ra) ( 0.) in 
    let i = Ba.set_3d vs i (  ra) (  ra) ( 0.) in     
    let i = Ba.set_3d vs i (-.ra) (  ra) ( 0.) in
    let i = ref i in 
    let k = Ba.seti_3d is 0 0 3 4 in 
    let k = Ba.seti_3d is k 0 4 5 in 
    let k = Ba.seti_3d is k 0 5 2 in 
    let k = Ba.seti_3d is k 0 2 3 in 
    let k = Ba.seti_3d is k 1 4 3 in 
    let k = Ba.seti_3d is k 1 5 4 in 
    let k = Ba.seti_3d is k 1 2 5 in 
    let k = Ba.seti_3d is k 1 3 2 in 
    let k = ref k in 
    (* For each face we split its edges in two, move the new points
       on the sphere and add the resulting faces to the index. Emap 
       allows us to make sure we don't split an edge more than once *) 
    for l = 1 to level do 
      let face_count = !k / 3 in
      let visited = ref Emap.empty (* maps edges to new vertex index. *) in
      for f = 0 to face_count - 1 do
        let fi = f * 3 in
        let p1i, p2i, p3i = Ba.geti_3d is fi in 
        let p1 = Ba.get_v3 vs (3 * p1i) in 
        let p2 = Ba.get_v3 vs (3 * p2i) in 
        let p3 = Ba.get_v3 vs (3 * p3i) in
        let subdivide p1i p2i p1 p2 = 
          let e = (p1i, p2i) in 
          try Emap.find e !visited with 
          | Not_found ->
              let pnewi = !i / 3 in
              i := Ba.set_v3 vs !i V3.(r * unit (P3.mid p1 p2));
              visited := Emap.add e pnewi !visited;
              pnewi
        in
        let pai = subdivide p1i p2i p1 p2 in 
        let pbi = subdivide p2i p3i p2 p3 in 
        let pci = subdivide p3i p1i p3 p1 in 
        k := Ba.seti_3d is !k p1i pai pci;
        k := Ba.seti_3d is !k pai p2i pbi;
        k := Ba.seti_3d is !k pbi p3i pci; 
        ignore (Ba.seti_3d is fi pai pbi pci)
      done
    done;
    let attrs = [Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray vs))]in 
    let index = Buf.create ~unsigned:true (`Bigarray is) in
    Prim.create ?name ~index `Triangles attrs
    
  let rect ?(name = "rect") ?tex ?(segs = Size2.unit) size =
    let do_tex = tex <> None in
    let xseg = Float.int_of_round (Size2.w segs) in
    let yseg = Float.int_of_round (Size2.h segs) in
    let attrs = 
      let xsegf = float xseg in 
      let ysegf = float yseg in 
      let dx = Size2.w size /. xsegf in 
      let dy = Size2.h size /. ysegf in 
      let x0 = -0.5 *. Size2.w size in 
      let y0 = -0.5 *. Size2.h size in
      let vertex_count = (xseg + 1) * (yseg + 1) in
      let tex_size = if do_tex then 0 else 2 * vertex_count in
      let b = Ba.create Bigarray.float32 (3 * vertex_count + tex_size) in
      let i = ref 0 in
      for y = 0 to yseg do 
        for x = 0 to xseg do 
          let y = float y in 
          let x = float x in
          i := Ba.set_3d b !i (x0 +. x *. dx) (y0 +. y *. dy) 0.; 
          if do_tex then i := Ba.set_2d b !i (x /. xsegf) (y /. ysegf);
        done
      done;
      let b = Buf.create (`Bigarray b) in      
      (Attr.create ~stride:2 ~first:0 Attr.vertex ~dim:3 b) :: 
      match tex with 
      | None -> []
      | Some tex -> [ Attr.create ~stride:3 ~first:3 tex ~dim:2 b ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (xseg * yseg * 2 * 3) in 
      let id x y = y * (xseg + 1) + x in 
      let i = ref 0 in
      for y = 0 to yseg - 1 do 
        for x = 0 to xseg - 1 do 
          i := Ba.set_3d b !i (id x y) (id (x+1) (y  )) (id (x+1) (y+1));
          i := Ba.set_3d b !i (id x y) (id (x+1) (y+1)) (id (x  ) (y+1));
        done
      done;
      Buf.create (`Bigarray b)
    in
    Prim.create ~name ~index `Triangles attrs 
end

module Effects = struct
  
  module Wireframe = struct

    let model_to_clip = Uniform.model_to_clip "model_to_clip" 
    let fill_color = Uniform.v4 "fill_color" Color.white
    let wire_color = Uniform.v4 "wire_color" Color.black
    let wire_width = Uniform.float "wire_width" 1.
    let wire_only = Uniform.bool "wire_only" false

    let create ?fill_color ?wire_color ?wire_width ?wire_only () = 
      failwith "TODO"
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
