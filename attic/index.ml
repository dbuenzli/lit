(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

type format = { first : int; range : (int * int) option } 

let format ?range ?(first = 0) () = 
  Check.igeq "first" first 0;
  Check.is_range range;
  { first = first; range = range }
   
let first f = f.first
let range f = f.range
let print_format fmt f =
  let pr s = Format.fprintf fmt s in
  let print_range f = function
    | None -> pr "None"
    | Some (min, max) -> pr "Some@ (%d,@ %d)" min max
  in
  pr "@[<hov 1>{first =@ %d;@ range =@ %a}@]" f.first print_range f.range

type 'a t = 'a Buf.t * format constraint 'a = [< Buf.uint_kind ]

let pos (b, f) i =  f.first + i 
let max (b, f) = (Buf.length b) - f.first
let print fmt ?pos ?len (b, f) = Buf.print_data fmt ?pos ?len b 

(* TODO put that were it should go. *)
let _gl_draw m ~count (b,f) = 
  let ft = Buf._ft b in
  let storage = Buf._gl_storage b in
  match f.range with
  | Some (min, max) -> 
      let c = Buf._gl_bind_index b f.first in
      Gl.draw_range_elements ft m min max count storage c 
  | None -> 
      let c = Buf._gl_bind_index b f.first in
      Gl.draw_elements ft m count storage c
