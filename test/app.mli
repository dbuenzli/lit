(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
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

type key_sym = [ 
    | `Alt of [ `Left | `Right ]
    | `Arrow of [ `Down | `Up | `Left | `Right ]
    | `Backspace
    | `Capslock
    | `Ctrl of [ `Left | `Right ]
    | `End
    | `Enter
    | `Escape
    | `Function of int
    | `Home
    | `Letter of char
    | `Digit of int
    | `Meta of [ `Left | `Right ] 
    | `Pagedown
    | `Pageup
    | `Return
    | `Shift
    | `Tab
    | `Unknown of int ] 

type key = [ `Down | `Up ] * key_sym

type mouse_button = [ `Left | `Right ]
type mouse = 
  [ `Button of [ `Down | `Up ] * mouse_button * p2 
  | `Motion of p2 * v2 ]

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

val select_backend : unit -> (module Lit.Renderer.T)

type config =  
  { gl : int * int; 
    tick_hz : int; 
    pos : v2; 
    size : size2; 
    name : string; 
    ev : t -> ev -> [ `Ok | `Error of string | `Yield | `Quit ] } 

val default : config 
(** [default] is the default configuration. TODO *)

val create : config -> t
(** [create config] *)

(** {1 Run} *) 

val run : t -> [ `Ok | `Error of string | `Quit ]
(** [run app] runs the app [name] using [ev] and [config]. *)

val handle_run : t -> unit 


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
