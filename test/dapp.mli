(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Basic app support.

    Basic application support for graphical demos. *)

open Gg

(** {1 Pixel format} *)

(** {1 Apps} *)

type t
(** The type for apps. *)


(** {1 Drawing} *)

val update_surface : t -> unit
(** [update_surface a] update's [a]'s rendering surface. *)


val surface_size : t -> size2

(** {1 Fullscreen} *)

val toggle_fullscreen : t -> unit
(** Switches between fullscreen and windowed mode. *)

val is_fullscreen : t -> bool
(** [is_fullscreen a] is [true] if [a] takes over the screen. *)

(** {1 Timing} *)

val elapsed : unit -> float
(** [elapsed ()] number of seconds elapsed since the beginning of the
    program. *)

val time : ('a -> 'b) -> 'a -> 'b * float
(** [time f x] is the number of seconds taken to execute f x. *)


(** {1 Events} *)

type mode = [ `Windowed | `Fullscreen ]

type env = [ `Init | `Exit | `Yield | `Resize of size2
           | `Mode of mode ]

type keysym =
  [ `Alt of [ `Left | `Right ]
  | `Arrow of [ `Up | `Down | `Left | `Right ]
  | `Backspace
  | `Ctrl of [ `Left | `Right ]
  | `Digit of int
  | `End
  | `Enter
  | `Escape
  | `Function of int
  | `Home
  | `Meta of [ `Left | `Right ]
  | `Page of [ `Up | `Down ]
  | `Return
  | `Shift of [ `Left | `Right ]
  | `Space
  | `Tab
  | `Uchar of int
  | `Unknown of int ]

val keysym_to_string : keysym -> string
val pp_keysym : Format.formatter -> keysym -> unit

type key = [ `Down | `Up ] * keysym

type mouse_button = [ `Left | `Right | `Middle | `X1 | `X2 ]
type mouse =
  [ `Button of [ `Down | `Up ] * mouse_button * p2
  | `Motion of p2 * v2 ]
(** Coordinates are in window normalized coordinates with
    (0,0) bot left and (1,1) top right. *)

type ev =
  [ `Env of env
  | `Tick of float
  | `Key of key
  | `Mouse of mouse
  | `Text of string ]
(** The type for events.
    {ul
    {- `Tick t, according to confg.tick_hz. The value [t]
        is monotonic time in s elapsed since the beginning from
        the program.}} *)

val text_ev : t -> bool
(** [text_ev a] is [true] if [`Text] events are delivered. *)

val set_text_ev : t -> bool -> unit
(** [set_text_ev t flag] sets [`Text] events to be delivered according
    to [flag]. *)

(** {1 Configuration} *)

type ev_ret = [ `Ok | `Error of string | `Yield | `Quit ]

val select_backend : unit -> (module Lit.Renderer.T)

type config =
  { hidpi : bool;
    gl : int * int;
    tick_hz : int;
    pos : v2;
    size : size2;
    name : string; }

val default : config
(** [default] is the default configuration. TODO *)

val create : config -> t
(** [create config] *)

val size : t -> size2
(** [size app] is [app]'s size. *)

(** {1 Run} *)

val run : t -> ev:(t -> ev -> ev_ret) -> [ `Ok | `Error of [`Msg of string] |
                                           `Quit ]
(** [run app] runs the app [name] using [ev] and [config]. *)

val handle_run : t -> ev:(t -> ev -> ev_ret) -> unit


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
