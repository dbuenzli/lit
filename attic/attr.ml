(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

type kind = [ `V1 | `V2 | `V3 | `V4 | `M2 | `M3 | `M4 ]
let kind_size = function
  | `V1 -> 1
  | `V2 -> 2
  | `V3 -> 3
  | (`V4 | `M2) -> 4
  | `M3 -> 9
  | `M4 -> 16

type 'a format = 
    { normalized : bool; 
      first : int; 
      skip : int; 
      kind : 'a;
      asize : int (* attribute kind size *) } 
constraint 'a = [< kind ]

let format ?(normalized = false) ?(first = 0) ?(skip = 0) kind = 
  Check.igeq "first" first 0; 
  Check.igeq "skip" skip 0;
  { normalized = normalized; 
    first = first; 
    skip = skip; 
    kind = kind; 
    asize = kind_size kind; }
    
let normalized f = f.normalized
let first f = f.first
let skip f = f.skip
let kind f = f.kind
let print_format fmt f = 
  let string_of_kind = function
    | `V1 -> "`V1"
    | `V2 -> "`V2"
    | `V3 -> "`V3"
    | `V4 -> "`V4"
    | `M2 -> "`M2"
    | `M3 -> "`M3"
    | `M4 -> "`M4"
  in
  Format.fprintf fmt 
    "@[<hov 1>{normalized =@ %b;@ first =@ %d;@ skip = %d;@ kind =@ %s}@]"
    f.normalized f.first f.skip (string_of_kind f.kind)  

type ('a, 'b) t = 'a Buf.t * 'b format 

let pos (b, f) i = f.first + (f.asize + f.skip) * i
let size (b, f) = f.asize
let max (b, f)  = 
  let real  =  f.asize + f.skip in
  let units = (Buf.length b) - f.first in
  (units / real) + ((units mod real) / f.asize)

let print fmt ?(pos = 0) ?(len = max_int) ((b, f) as a) =
  let max_els = max a in
  let len' = if len = max_int then max_els - pos else len in
  Check.irange "pos" pos 0 max_els;
  Check.irange "len" len' 0 (max_els - pos);
  let print k el_fmt b = 
    let pr s = Format.fprintf fmt s in
    let iter a = 
      let pr_vec a pos = 
	pr "@[<1> (";
	pr el_fmt a.{pos};
	for i = pos + 1 to pos + f.asize - 1 do
	  pr " ";
	  pr el_fmt a.{i}
	done;
	pr ")@]"
      in
      let index i = f.first + i * (f.asize + f.skip) in
      pr_vec a (index pos);
      for i = pos + 1 to pos + len' - 1 do 
	pr ";@,@ ";
	pr_vec a (index i)
      done
    in
    pr "@[<1>["; if len' > 0 then Buf.map k `Read iter b; pr "]@]"
  in
  match Buf.kind b with
  | `Int8 -> print Buf.int8 "%d" (Buf.cast `Int8 b)
  | `Uint8 -> print Buf.uint8 "%d" (Buf.cast `Uint8 b)
  | `Int16 -> print Buf.int16 "%d" (Buf.cast `Int16 b)
  | `Uint16 -> print Buf.uint16 "%d" (Buf.cast `Uint16 b)
  | `Int32 -> print Buf.int32 "%ld" (Buf.cast `Int32 b)
  | `Uint32 -> print Buf.uint32 "%lu" (Buf.cast `Uint32 b)
  | `Float32 -> print Buf.float32 "%F" (Buf.cast `Float32 b)
  | `Float64 -> print Buf.float64 "%F" (Buf.cast `Float64 b)

(* TODO remove put that were it should go, maybe Buf._ft can be removed  *)
let _gl_source k (b,f) =
  let ft = Buf._ft b in
  Gl.enable_client_state ft k;
  let storage = Buf._gl_storage b in
  let stride = (f.asize + f.skip) * Buf._kind_bytes (Buf.kind b) in
  let c = Buf._gl_bind_attribute b f.first in
  if k = Gl.vertex_array then Gl.vertex_pointer ft f.asize storage stride c else 
  if k = Gl.normal_array then Gl.normal_pointer ft storage stride c else 
  if k = Gl.color_array then Gl.color_pointer ft f.asize storage stride c else 
  if k = Gl.texture_coord_array then 
    Gl.tex_coord_pointer ft f.asize storage stride c
      
let _gl_unsource ft k = Gl.disable_client_state ft k
