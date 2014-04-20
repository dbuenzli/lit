(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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
