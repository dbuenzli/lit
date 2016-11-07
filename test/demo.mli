(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Lit

(** A few common, ad-hoc, tools for Lit's demos. *)

(** {1 Primitive cycler} *)

val prim_cycler :
  ?normals:bool -> ?prims:(Prim.t Lazy.t) list -> unit -> (unit -> Prim.t)

(** {1 Default commands} *)

type cmd =
  [ `Init | `Exit | `Resize of size2 | `Tick of float
  | `Toggle_fullscreen | `Cycle_prim | `None of Dapp.ev | `Move_in | `Move_out ]

val command_of_key : Dapp.keysym ->
  [> `Toggle_fullscreen | `Cycle_prim | `Exit | `Move_in | `Move_out ] option

val event_to_command : Dapp.ev -> cmd

val ev_of_command_handler :
  (Dapp.t -> [> cmd ] -> Dapp.ev_ret) -> Dapp.t -> Dapp.ev -> Dapp.ev_ret

val default_size : size2

(** {1 Terminal output} *)

val show_start : Lit.Renderer.t -> unit
(** [show_start r] prints basic information about [r] on [stdout]. *)

val show_stats : float -> ('a -> unit) -> 'a -> ('b -> unit) -> 'b -> unit
(** [show_stats t draw v update v'] calls and measures the time taken
    by [draw v] and [update v'] and uses the absolute time [t] to compute
    the time between two calls to [show_timings]. All these timings
    are reported by overwriting the last line of [stdout]. *)

val show_stop : unit -> unit
(** [show_stop ()] just prints a final newline on [stdout]. *)

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
