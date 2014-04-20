(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Lit
open Gg

let pp = Format.printf 

let default_size = Size2.v 600. 400.

(* Primitive cycler *) 

let prim_cycler ?(normals = false) ?prims () = 
  let prims () = match prims with 
  | Some [] -> invalid_arg "prims is the empty list"
  | Some prims -> prims 
  | None -> 
      let with_normals p = if normals then Litu.Prim.with_normals p else p in
      [ lazy (with_normals (Litu.Prim.cube 1.)); 
        lazy (with_normals (Litu.Prim.sphere ~level:4 0.5)); 
        lazy (with_normals (Litu.Prim.rect (`Size (Size2.v 1. 1.))));
        lazy (with_normals (Bunny.create ~scale:1.5 ())) ]
  in
  let cycle = ref (prims ()) in
  let rec loop () = match !cycle with 
  | [] -> cycle := prims (); loop () 
  | p :: ps -> cycle := ps; Lazy.force p
  in
  loop 

(* Default commands *) 

type cmd = 
  [ `Init | `Exit | `Resize of size2 | `Tick of float 
  | `Toggle_fullscreen | `Cycle_prim | `None of App.ev | `Move_in | `Move_out ]

let command_of_key = function
| `Escape -> Some `Exit
| `Space -> Some `Toggle_fullscreen
| `Digit 1 -> Some `Cycle_prim
| `Arrow `Up -> Some `Move_in 
| `Arrow `Down -> Some `Move_out
| _ -> None

let event_to_command = function
| `Env (`Init | `Exit | `Resize _ as cmd) -> cmd
| `Key (`Up, k) as e -> 
    begin match command_of_key k with 
    | Some cmd -> cmd
    | None -> `None e
    end
| `Tick _ as cmd -> cmd
| e -> `None e

let ev_of_command_handler cmd_handler =
  fun app e -> cmd_handler app (event_to_command e)

(* Terminal output *) 

let show_start r = 
  pp "@[%a@]@." Renderer.Cap.pp_gl_synopsis r 

let last_show_stats = ref 0.
let show_stats now draw v update v' = 
  let (), draw = App.time draw v in 
  let (), swap = App.time update v' in 
  let now = now *. 1e6 in
  let swap = swap *. 1e6 in 
  let draw = draw *. 1e6 in
  Printf.printf "\r\x1B[K\
                 frame:\x1B[01m%4.0fμs\x1B[m \
                 (draw:%4.0fμs update:%4.0fμs) dt:%5.0fμs%!"
    (swap +. draw) draw swap (now -. !last_show_stats); 
  last_show_stats := now

let show_stop () = Printf.printf "\n%!"

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
