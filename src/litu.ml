(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Lit


let str = Format.sprintf

(* Invalid argument strings. *) 

let err_no_cpu_buf b = str "%s: no CPU buffer" b
let err_dim k dim = str "%s: unsupported dimension (%d)" k dim
let err_prim_kind k = 
  str "unsupported primitive kind (%s)" (Prim.kind_to_string k)

let err_buf_scalar_type k st = 
  str "%s: unsupported buffer scalar type (%s)" k 
    (Buf.scalar_type_to_string st)

(* Primitives *) 

module Prim = struct

  let pusher set b = 
    let i = ref 0 in 
    fun x y z -> set b !i x y z; i := !i + 3

  let rect ?name ?tex ?(segs = Size2.unit) spec =
    let do_tex = tex <> None in
    let xseg = Float.int_of_round (Size2.w segs) in
    let yseg = Float.int_of_round (Size2.h segs) in
    let attrs = 
      let xsegf = float xseg in 
      let ysegf = float yseg in 
      let dx, dy, x0, y0 = match spec with 
      | `Size s -> 
          let dx = Size2.w s /. xsegf in 
          let dy = Size2.h s /. ysegf in 
          let x0 = -0.5 *. Size2.w s in 
          let y0 = -0.5 *. Size2.h s in
          dx, dy, x0, y0
      | `Box b -> 
          let dx = Size2.w (Box2.size b) /. xsegf in 
          let dy = Size2.h (Box2.size b) /. ysegf in
          let x0 = Box2.minx b in 
          let y0 = Box2.miny b in 
          dx, dy, x0, y0
      in
      let vertex_count = (xseg + 1) * (yseg + 1) in
      let tex_size = if do_tex then 2 * vertex_count else 0 in
      let b = Ba.create Bigarray.float32 (3 * vertex_count + tex_size) in
      let i = ref 0 in
      for y = 0 to yseg do 
        for x = 0 to xseg do 
          let y = float y in 
          let x = float x in
          Ba.set_3d b !i (x0 +. x *. dx) (y0 +. y *. dy) 0.; i := !i + 3;
          if do_tex then (Ba.set_2d b !i (x /. xsegf) (y /. ysegf); i := !i + 2)
        done
      done;
      let b = Buf.create (`Bigarray b) in
      let stride = if do_tex then 5 else 3 in
      (Attr.create ~stride ~first:0 Attr.vertex ~dim:3 b) :: 
      match tex with 
      | None -> []
      | Some tex -> [ Attr.create ~stride:5 ~first:3 tex ~dim:2 b ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (xseg * yseg * 2 * 3) in 
      let id x y = y * (xseg + 1) + x in 
      let push = pusher Ba.set_3d b in
      for y = 0 to yseg - 1 do 
        for x = 0 to xseg - 1 do 
          push (id x y) (id (x+1) (y  )) (id (x+1) (y+1));
          push (id x y) (id (x+1) (y+1)) (id (x  ) (y+1))
        done
      done;
      Buf.create (`Bigarray b)
    in
    Prim.create ?name ~index `Triangles attrs 

  let cuboid_extrema = function
  | `Size s -> 
      let hw, hh, hd = V3.(to_tuple (0.5 * s)) in
      (-.hw, -.hh, hd), (hw, hh, -.hd)
  | `Box b -> 
      if Box3.is_empty b then (0., 0., 0.), (0., 0., 0.) else
      V3.to_tuple (Box3.min b), V3.to_tuple (Box3.max b)

  let cuboid_dups ?name spec = 
    let attrs =
      let ba = Ba.create Bigarray.float32 (8 * 3 * 3) in 
      let push = pusher Ba.set_3d ba in 
      let (l, b, n), (r, t, f) = cuboid_extrema spec in
      push l b n; push r b n; push r t n; push l t n; (* Near *)
      push l b n; push r b n; push r b f; push l b f; (* Bottom *)
      push l b n; push l b f; push l t f; push l t n; (* Left *)
      push r b n; push r b f; push r t f; push r t n; (* Right *)
      push r t n; push r t f; push l t f; push l t n; (* Top *)
      push l t f; push l b f; push r b f; push r t f; (* Far *)
      [ Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray ba)) ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (6 * 2 * 3) in
      let push = pusher Ba.set_3d b in
      push  0  2  3; push  0  1  2; (* Near *)
      push  4  7  6; push  4  6  5; (* Bottom *)
      push  8 11 10; push  8 10  9; (* Left *)
      push 12 14 15; push 12 13 14; (* Right *) 
      push 16 17 18; push 16 18 19; (* Top *)
      push 20 23 22; push 20 22 21; (* Far *)
      
      Buf.create (`Bigarray b)
    in
    Prim.create ?name ~index `Triangles attrs 

  let cuboid_no_dups ?name spec = 
    let attrs = 
      let ba = Ba.create Bigarray.float32 (3 * 8) in 
      let push = pusher Ba.set_3d ba in
      let (l, b, n), (r, t, f) = cuboid_extrema spec in
      push l b n; push r b n; push l t n; push r t n; (* Near *)
      push l b f; push r b f; push l t f; push r t f; (* Far *) 
      [ Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray ba)) ]
    in
    let index = 
      let b = Ba.create Bigarray.int8_unsigned (6 * 2 * 3) in
      let push = pusher Ba.set_3d b in
      push 0 3 2; push 0 1 3; (* Front *) 
      push 0 5 1; push 0 4 5; (* Bottom *)
      push 0 6 4; push 0 2 6; (* Left *)
      push 1 7 3; push 1 5 7; (* Right *)
      push 2 7 6; push 2 3 7; (* Top *) 
      push 4 7 5; push 4 6 7; (* Far *)
      Buf.create (`Bigarray b)
    in
    Prim.create ?name ~index `Triangles attrs 

  let cuboid ?name ?(dups = true) spec =
    if dups then cuboid_dups ?name spec else cuboid_no_dups ?name spec

  let cube ?name ?dups s = cuboid ?name ?dups (`Size (Size3.v s s s))

  (* Sphere *) 

  module Edge = struct
    type t = int * int                     (* vertex indices for the edges. *) 
    let compare (v0, v1 as v) (u0, u1 as u) =  (* insensitive to direction. *) 
      let v = if v0 < v1 then v else v1, v0 in 
      let u = if u0 < u1 then u else u1, u0 in 
      compare v u
  end 
  module Emap = Map.Make (Edge)

  let sphere ?name ?(level = 4) r =
    let ra = r /. sqrt 2. in 
    let four_pow n = 1 lsl (2 * n) in
    let vertex_count = 2 + four_pow (level + 1) in 
    let face_count = 8 * four_pow level in
    let vs = Ba.create Bigarray.float32 (vertex_count * 3) in 
    let vs_i = ref 0 in 
    let vs_push x y z = Ba.set_3d vs !vs_i x y z; vs_i := !vs_i + 3 in
    let is = Ba.create Bigarray.int32 (face_count * 3) in
    let is_i = ref 0 in 
    let is_push x y z = Ba.seti_3d is !is_i x y z; is_i := !is_i + 3 in
    (* Level 0 isocahedron *)
    vs_push (  0.) (  0.) (  r);
    vs_push (  0.) (  0.) (-.r);
    vs_push (-.ra) (-.ra) ( 0.);
    vs_push (  ra) (-.ra) ( 0.);
    vs_push (  ra) (  ra) ( 0.);
    vs_push (-.ra) (  ra) ( 0.);
    is_push 0 3 4;
    is_push 0 4 5;
    is_push 0 5 2; 
    is_push 0 2 3;
    is_push 1 4 3; 
    is_push 1 5 4; 
    is_push 1 2 5; 
    is_push 1 3 2;
    (* For each face we split its edges in two, move the new points
       on the sphere and add the resulting faces to the index. Emap 
       allows us to make sure we don't split an edge more than once *) 
    for l = 1 to level do 
      let face_count = !is_i / 3 in
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
              let pnewi = !vs_i / 3 in
              Ba.set_v3 vs !vs_i V3.(r * unit (P3.mid p1 p2));
              vs_i := !vs_i + 3;
              visited := Emap.add e pnewi !visited;
              pnewi
        in
        let pai = subdivide p1i p2i p1 p2 in 
        let pbi = subdivide p2i p3i p2 p3 in 
        let pci = subdivide p3i p1i p3 p1 in 
        is_push p1i pai pci;
        is_push pai p2i pbi;
        is_push pbi p3i pci;
        Ba.seti_3d is fi pai pbi pci
      done
    done;
    let attrs = [Attr.create Attr.vertex ~dim:3 (Buf.create (`Bigarray vs))]in 
    let index = Buf.create ~unsigned:true (`Bigarray is) in
    Prim.create ?name ~index `Triangles attrs
    
  (* Functions *) 
  
  let vertex_index_iterator count idx = 
    (* That's not going to be very efficient... *) 
    let max = count - 1 in
    let i = ref (-1) in
    match idx with 
    | Some b when not (Buf.cpu_exists b) -> 
        invalid_arg (err_no_cpu_buf "index")
    | None -> 
        (* Indices are the linear count. *) 
        fun () -> if !i = max then assert false else (incr i; !i)
    | Some b -> 
        let get_cpu b k = match Buf.cpu b k with 
        | Some ba -> ba 
        | None -> assert false 
        in
        match Buf.scalar_type b with 
        | `UInt8 -> 
            let ba = get_cpu b Bigarray.int8_unsigned in 
            fun () -> if !i = max then assert false else (incr i; ba.{!i})
        | `UInt16 -> 
            let ba = get_cpu b Bigarray.int16_unsigned in 
            fun () -> if !i = max then assert false else (incr i; ba.{!i})
        | `UInt32 -> 
            (* That's actually the culprit, it's annoying. *) 
            let ba = get_cpu b Bigarray.int32 in
            fun () -> if !i = max then assert false else 
              (incr i; Int32.to_int (ba.{!i}))
        | _ -> assert false (* This is guaranted by Prim.create *) 
            
                
  let do_normals : tri_count:int -> index:(unit -> int) -> 
    vs:(float, 'c) bigarray -> vs_first:int -> vs_stride:int -> 
    ns:(float, 'd) bigarray -> unit = 
    fun ~tri_count ~index ~vs ~vs_first ~vs_stride ~ns ->
    let count = Bigarray.Array1.dim ns in
    for i = 0 to count - 1 do ns.{i} <- 0. done;
    for i = 0 to tri_count - 1 do
      let vi1 = index () in 
      let vi2 = index () in 
      let vi3 = index () in 
      let v1 = Ba.get_v3 vs (vs_first + vi1 * vs_stride) in 
      let v2 = Ba.get_v3 vs (vs_first + vi2 * vs_stride) in 
      let v3 = Ba.get_v3 vs (vs_first + vi3 * vs_stride) in
      let n = V3.(cross (v2 - v1) (v3 - v1)) in
      Ba.set_v3 ns (vi1 * 3) (V3.add n (Ba.get_v3 ns (vi1 * 3))); 
      Ba.set_v3 ns (vi2 * 3) (V3.add n (Ba.get_v3 ns (vi2 * 3))); 
      Ba.set_v3 ns (vi3 * 3) (V3.add n (Ba.get_v3 ns (vi3 * 3)));
    done; 
    for i = 0 to (count / 3) - 1 do 
      Ba.set_v3 ns (i * 3) (V3.unit (Ba.get_v3 ns (i * 3)))
    done
    
  let with_normals ?(scalar_type = `Float32)  ?name p =
    let kind = Prim.kind p in 
    if kind <> `Triangles then invalid_arg (err_prim_kind kind) else 
    let index, tri_count = match Prim.index p with 
    | Some b when not (Buf.cpu_exists b) -> invalid_arg (err_no_cpu_buf "index")
    | Some _ | None as i -> 
        let count = Prim.count_now p in
        vertex_index_iterator count i, count / 3
    in
    let vs = Prim.get p Attr.vertex (* raises if not found *) in
    let dim = Attr.dim vs in 
    if dim <> 3 then invalid_arg (err_dim "vertex attribute" dim) else
    let vs_first = Attr.first vs in 
    let vs_stride = Attr.stride vs in
    let vs = Attr.buf vs in
    let ns = 
      let do_it vs_ba_kind ns_ba_kind = match Buf.cpu vs vs_ba_kind with 
      | None -> invalid_arg (err_no_cpu_buf "vertex attribute")
      | Some vs -> 
          let count = 
            (Ba.length vs + (vs_stride - dim) - vs_first) / vs_stride
          in
          let ns = Ba.create ns_ba_kind (3 * count) in 
          do_normals ~index ~tri_count ~vs ~vs_first ~vs_stride ~ns; 
          Attr.create Attr.normal ~dim:3 (Buf.create (`Bigarray ns))
      in
      match Buf.scalar_type vs, scalar_type with 
      | `Float32, `Float32 -> do_it Bigarray.float32 Bigarray.float32
      | `Float32, `Float64 -> do_it Bigarray.float32 Bigarray.float64
      | `Float64, `Float32 -> do_it Bigarray.float64 Bigarray.float32
      | `Float64, `Float64 -> do_it Bigarray.float64 Bigarray.float64
      | st, _ -> invalid_arg (err_buf_scalar_type "vertex attribute" st)
    in
    let attrs = 
      let not_normal a = Attr.name a <> Attr.normal in
      ns :: (List.filter not_normal (Prim.attrs p)) 
    in
    let tr = Prim.tr p in
    let first = Prim.first p in
    let count = Prim.count p in 
    let index = Prim.index p in
    Prim.create ?name ~tr ~first ?count ?index kind attrs
        
end

module Effect = struct

  module Wireframe = struct

    (* Effect adapted from http://cgg-journal.com/2008-2/06/index.html. *)

    module U = struct 
      let model_to_clip = Uniform.model_to_clip "model_to_clip" 
      let viewport_size = Uniform.viewport_size "vp_size" 
      let fill_color = Uniform.v4 "fill_color" Color.white
      let wire_color = Uniform.v4 "wire_color" Color.black
      let wire_width = Uniform.float "wire_width" 1.
      let wire_only = Uniform.bool "wire_only" false
    end

    let uset = 
      Uniform.(empty + U.model_to_clip + U.viewport_size + U.fill_color + 
               U.wire_color + U. wire_width + U.wire_only)
               
    let program = Prog.create ~uset [
        Prog.shader `Vertex "
        uniform mat4 model_to_clip;
        in vec4 vertex;
        out vec4 v_vertex;
        void main() { gl_Position = model_to_clip * vertex; }"; 

        Prog.shader `Geometry "
        layout(triangles) in;
        layout(triangle_strip, max_vertices=3) out;

        uniform vec2 vp_size;
        noperspective out vec3 dist;

        void main(void)
        {
           vec2 p0 = vp_size * gl_in[0].gl_Position.xy/gl_in[0].gl_Position.w;
           vec2 p1 = vp_size * gl_in[1].gl_Position.xy/gl_in[1].gl_Position.w;
           vec2 p2 = vp_size * gl_in[2].gl_Position.xy/gl_in[2].gl_Position.w;
  
           vec2 v0 = p2 - p1;
           vec2 v1 = p2 - p0;
           vec2 v2 = p1 - p0;
           float area = abs(v1.x * v2.y - v1.y * v2.x);

           dist = vec3(area / length(v0), 0, 0);
           gl_Position = gl_in[0].gl_Position;
           EmitVertex();
	
           dist = vec3(0, area / length(v1), 0);
           gl_Position = gl_in[1].gl_Position;
           EmitVertex();

           dist = vec3(0, 0, area / length(v2));
           gl_Position = gl_in[2].gl_Position;
           EmitVertex();

           EndPrimitive();
         }"; 

        Prog.shader `Fragment "
        uniform bool wire_only;
        uniform float wire_width;
        uniform vec4 wire_color;
        uniform vec4 fill_color; 

        noperspective in vec3 dist;
        out vec4 color;

        void main(void)
        {
          float d = min(dist[0],min(dist[1],dist[2]));
          float I = exp2(-(2 / wire_width) * d * d);
          if (wire_only)
          {
            color = I * wire_color;
          } else {
            color = vec4(I * wire_color.rgb + (1.0 - I) * fill_color.rgb, 
                         fill_color.a);
          }
        }" ]
          
    let create ?raster ?depth ?fill_color ?wire_color ?wire_width 
        ?wire_only () = 
      let effect = Effect.create ?raster ?depth program in
      let set k v = match v with
      | None -> () | Some v -> Effect.set_uniform effect k v
      in
      set U.fill_color fill_color;
      set U.wire_color wire_color;
      set U.wire_width wire_width;
      set U.wire_only wire_only;
      effect
      
    include U
  end
end


module Manip = struct

  open Gg

  type rot = 
    { center : p2; 
      radius : float; 
      start : p3; 
      init : quat }
    
  let sphere_point r c pt = 
    let inv_r = 1. /. r in
    let p = V2.(inv_r * (pt - c)) in 
    let d = V2.norm2 pt in 
    if d <= 1. then V3.of_v2 p ~z:(sqrt (1. -. d)) else 
    let a = 1. /. sqrt d in 
    V3.of_v2  V2.(a * p) ~z:0.0 
      
  let rot ?(center = P2.o) ?(radius = 1.0) ?(init = Quat.id) ~start () = 
    { center; radius; start = sphere_point radius center start; init }
    
  let rot_update r pt =
    let p = sphere_point r.radius r.center pt in 
    let q = V4.of_v3 V3.(cross r.start p) ~w:V3.(dot r.start p) in
    Quat.mul q r.init
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
