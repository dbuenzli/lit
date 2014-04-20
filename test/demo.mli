(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Lit

(** A few common, ad-hoc, tools for Lit's demos. *) 


(** {1 Primitive cycler} *) 

val prim_cycler : ?normals:bool -> ?prims:(Prim.t Lazy.t) list -> unit -> 
  (unit -> Prim.t)

(** {1 Default commands} *) 

type cmd = 
  [ `Init | `Exit | `Resize of size2 | `Tick of float 
  | `Toggle_fullscreen | `Cycle_prim | `None of App.ev | `Move_in | `Move_out ]

val command_of_key : App.keysym -> 
  [> `Toggle_fullscreen | `Cycle_prim | `Exit | `Move_in | `Move_out ] option

val event_to_command : App.ev -> cmd 

val ev_of_command_handler : 
  (App.t -> [> cmd ] -> App.ev_ret) -> App.t -> App.ev -> App.ev_ret

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
