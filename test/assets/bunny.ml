(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* The stanford bunny. *)

open Gg
open Lit

(* TODO this can be useful in litu. *) 
let aabb_normalize verts count = 
  let minx = ref (+. max_float) in
  let miny = ref (+. max_float) in
  let minz = ref (+. max_float) in
  let maxx = ref (-. max_float) in
  let maxy = ref (-. max_float) in
  let maxz = ref (-. max_float) in
  for k = 0 to count - 1 do 
    let x = verts.{k * 3 + 0} in 
    let y = verts.{k * 3 + 1} in 
    let z = verts.{k * 3 + 2} in 
    minx := if x < !minx then x else !minx; 
    miny := if y < !miny then y else !miny; 
    minz := if z < !minz then z else !minz; 
    maxx := if x > !maxx then x else !maxx; 
    maxy := if y > !maxx then y else !maxy; 
    maxz := if z > !maxz then z else !maxz;
  done; 
  let w = !maxx -. !minx in 
  let h = !maxy -. !miny in 
  let d = !maxz -. !minz in
  let scale = 1. /. (max (max w h) d) in 
  let center = V3.v 
      (-0.5 *. (!minx +. !maxx))
      (-0.5 *. (!miny +. !maxy))
      (-0.5 *. (!minz +. !maxz))
  in
  M4.mul (M4.scale3 (V3.v scale scale scale)) (M4.move3 center)

let create ?file ?scale () = 
  let vcount = 35947 in 
  let fcount = 69451 in 
  let buf = Buf.create (`Cpu (`Float32, vcount * 3)) in
  let verts = Attr.create Attr.vertex ~dim:3 buf in
  let index = Buf.create (`Cpu (`UInt16, fcount * 3)) in 
  let prim = Prim.create `Triangles ~index [ verts ] in
  let v = Buf.get_cpu buf Ba.Float32 in
  let i = Buf.get_cpu index Ba.UInt16 in
  let locs = [ "test/assets/bunny.dat"; "assets/bunny.dat"; "bunny.dat" ] in
  try
    let fname = match file with 
    | None -> List.find Sys.file_exists locs
    | Some f -> f
    in
    try
      let ic = open_in fname in
      let push b i convert s = 
        let sp1 = String.index s ' ' in 
        let sp2 = String.rindex s ' ' in 
        let x = String.sub s 0 sp1 in 
        let y = String.sub s (sp1 + 1) (sp2 - sp1 - 1) in
        let z = String.sub s (sp2 + 1) (String.length s - sp2 - 1) in 
        Ba.set_3d b i (convert x) (convert y) (convert z); 
      in
      for k = 0 to vcount - 1 do 
        push v (k * 3) float_of_string  (input_line ic)
      done; 
      for k = 0 to fcount - 1 do 
        push i (k * 3) int_of_string (input_line ic)
      done;
      close_in ic;
      match scale with 
      | None -> prim 
      | Some s ->
          let m = aabb_normalize v vcount in 
          let tr = M4.mul (M4.scale3 (V3.v s s s)) m in
          Prim.create ~tr `Triangles ~index [ verts ]
    with 
    | Sys_error e | Failure e -> (Printf.eprintf "%s: %s%!\n" fname e; prim)
    | End_of_file -> (Printf.eprintf "%s: unexpected eof\n%!" fname; prim)
  with
  | Not_found -> 
      Printf.eprintf "None exist: %s\n%!" (String.concat ", " locs); prim

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
