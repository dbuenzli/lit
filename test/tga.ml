(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg

(** See e.g. http://www.paulbourke.net/dataformats/tga/ *) 

let write_header oc size typ = 
  let w = Float.int_of_round (Size2.w size) in 
  let h = Float.int_of_round (Size2.h size) in 
  let typ, psize = match typ with
  | `Color_rgb -> 2, 24
  | `Color_rgba -> 2, 32
  | `Gray -> 3, 8
  in
  output_byte oc 0; 
  output_byte oc 0;
  output_byte oc typ;
  output_byte oc 0; output_byte oc 0; 
  output_byte oc 0; output_byte oc 0; 
  output_byte oc 0; 
  output_byte oc 0; output_byte oc 0; 
  output_byte oc 0; output_byte oc 0; 
  output_byte oc (w land 0x00FF); 
  output_byte oc (w land 0xFF00 / 256);
  output_byte oc (h land 0x00FF); 
  output_byte oc (h land 0xFF00 / 256);
  output_byte oc psize; 
  output_byte oc 0;
  ()
  
let write_rgb oc ba = 
  for k = 0 to (Ba.length ba / 3) - 1 do
    output_byte oc ba.{k * 3 + 2}; 
    output_byte oc ba.{k * 3 + 1}; 
    output_byte oc ba.{k * 3 + 0}; 
  done

let write_rgba oc ba = 
  for k = 0 to (Ba.length ba / 4) - 1 do
    output_byte oc ba.{k * 4 + 2}; 
    output_byte oc ba.{k * 4 + 1}; 
    output_byte oc ba.{k * 4 + 0}; 
    output_byte oc ba.{k * 4 + 3}; 
  done

let write_gray oc ba = 
  for k = 0 to Ba.length ba - 1 do
    output_byte oc (255 - ba.{k}); 
  done

let write fname typ size ba = 
  try
    let oc = open_out_bin fname in 
    try 
      write_header oc size typ;
      begin match typ with 
      | `Color_rgb -> write_rgb oc ba 
      | `Color_rgba -> write_rgba oc ba 
      | `Gray -> write_gray oc ba 
      end; 
      `Ok (close_out oc) 
    with Sys_error _ as e -> close_out oc; raise e
  with Sys_error e -> `Error e

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
