(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
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
        lazy (with_normals (Litu.Prim.rect (Box2.v_mid P2.o (Size2.v 1. 1.))));
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
  | `Toggle_fullscreen | `Cycle_prim | `None of Dapp.ev | `Move_in | `Move_out ]

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
  let (), draw = Dapp.time draw v in
  let (), swap = Dapp.time update v' in
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
