(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg
open Tsdl

let str = Printf.sprintf 
let ( >>= ) x f = match x with
| `Error _ as e -> e
| `Ok v -> f v

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


let select_backend () = (module Lit_gl3 : Lit.Renderer.T)

(* Configuration *)

type config =  
  { gl : int * int; 
    tick_hz : int; 
    pos : v2; 
    size : size2; 
    name : string; 
    ev : t -> ev -> [ `Ok | `Error of string | `Yield | `Quit ] } 

and t = { 
  log : 'a. ('a, Format.formatter, unit) format -> 'a;
  init : [ `Ok of Sdl.window * Sdl.gl_context | `Error of string ];
  mutable state : [ `Ok | `Error of string | `Yield | `Quit ];
  mutable text_ev : bool;
  mutable is_fullscreen : bool;
  config : config; }


let window a = match a.init with 
| `Ok (win, _) -> Some win
| _ -> None

let is_fullscreen a = a.is_fullscreen
let text_ev a = a.text_ev 
let set_text_ev a f = 
  a.text_ev <- f; 
  if f then Sdl.start_text_input () else Sdl.stop_text_input () 

    
let default = 
  let exec = Filename.(chop_extension (basename Sys.argv.(0))) in
  { gl = 3,2; 
    tick_hz = 0;
    pos = V2.neg_infinity;
    size = V2.v 600. 400.;
    name = String.capitalize exec;
    ev = fun _ _ -> `Ok }


(* Timing *) 

let pfreqi = Sdl.get_performance_frequency ()
let pfreq = Int64.to_float pfreqi
let tick_now () = Sdl.get_performance_counter ()
let tick_start = tick_now ()
let elapsed_of_ticks now = Int64.(to_float (sub now tick_start)) /. pfreq
let tick_period hz = 
  if hz = 0 then Int64.max_int else Int64.div pfreqi (Int64.of_int hz)

let tick_next now period = 
  let part = Int64.(rem now period) in
  Int64.(add (sub now part) period)

let dur_to_next_ms now next = 
  if next = Int64.max_int then Int64.max_int else
  Int64.(div (mul (sub next now) 1000L) pfreqi)

let elapsed () = elapsed_of_ticks (tick_now ())
let time f x = 
  let start = tick_now () in 
  let v = f x in
  let dt = Int64.sub (tick_now ()) start in
  v, Int64.(to_float dt) /. pfreq


(* Window management *) 

let create_window c =
  let x, y = match c.pos with
  | pos when pos == V2.neg_infinity -> None, None 
  | pos -> Some (truncate (V2.x pos)), Some (truncate (V2.y pos)) 
  in
  let w, h = truncate (Size2.w c.size), truncate (Size2.h c.size) in
  let w_atts = Sdl.Window.(opengl + resizable) in
  let set a v = Sdl.gl_set_attribute a v in
  set Sdl.Gl.context_profile_mask Sdl.Gl.context_profile_core >>= fun () -> 
  set Sdl.Gl.context_major_version (fst c.gl)                 >>= fun () -> 
  set Sdl.Gl.context_minor_version (snd c.gl)                 >>= fun () -> 
  set Sdl.Gl.doublebuffer 1                                   >>= fun () ->
  Sdl.create_window ?x ?y ~w ~h c.name w_atts                 >>= fun win -> 
  Sdl.gl_create_context win                                   >>= fun ctx -> 
  Sdl.gl_make_current win ctx                                 >>= fun () ->
  Sdl.gl_set_swap_interval 0 >>= fun () -> 
  `Ok (win, ctx)
    
let destroy_window win ctx =
  Sdl.gl_delete_context ctx;
  Sdl.destroy_window win;
  `Ok ()

let toggle_fullscreen app = match app.init with
| `Error _ -> ()
| `Ok (win, _) ->
    let flags = Sdl.get_window_flags win in
    let f = match Sdl.Window.test flags Sdl.Window.fullscreen_desktop with
    | true -> app.is_fullscreen <- false; Sdl.Window.windowed
    | false -> app.is_fullscreen <- true; Sdl.Window.fullscreen_desktop 
    in
    match Sdl.set_window_fullscreen win f with 
    | `Ok () -> () 
    | `Error msg -> app.log "@[%s@]@." msg
                  
(* Keyboard *)

let callback_ev app e = app.state <- app.config.ev app e

let key_up app e = callback_ev app (`Key (`Up, `Home))
let key_down app e = 
  if Sdl.Event.(get e keyboard_repeat) <> 0 then () else
  callback_ev app (`Key (`Down, `Home))

let text_editing app e = 
  let text = Sdl.Event.(get e text_editing_text) in 
  let start = Sdl.Event.(get e text_editing_start) in 
  let len = Sdl.Event.(get e text_editing_length) in 
  Printf.printf "TODO Text editing ! t:%s start:%d len:%d" text start len
    
let text_input app e = 
  let text = Sdl.Event.(get e text_input_text) in
  callback_ev app (`Text text)

let keyboard app e =
  let key_scancode e = Sdl.Scancode.enum Sdl.Event.(get e keyboard_scancode) in
  match key_scancode e with 
  | `Escape -> `Exit
  | `Space -> toggle_fullscreen app; `Ok 
  | _ -> `Ok

let do_event app e = 
  let event e = Sdl.Event.(enum (get e typ)) in
  let window_event e = Sdl.Event.(window_event_enum (get e window_event_id)) in
  let window_size e = Sdl.Event.(get e window_data1, get e window_data2) in
  match event e with 
  | `Quit -> app.state <- `Quit
  | `Key_down -> 
        if keyboard app e = `Exit then app.state <- `Quit else 
        (ignore (key_down app e))
  | `Key_up -> ignore (key_up app e)
  | `Window_event -> 
      begin match window_event e with 
      | `Exposed (* TODO w,h = 0 ? *)| `Resized -> 
          let w, h = window_size e in
          let size = Size2.v (Int32.to_float w) (Int32.to_float h) in
          callback_ev app (`Env (`Resize size));
      | _ -> ()
      end
  | `Text_editing -> text_editing app e
  | `Text_input -> ignore (text_input app e)
  | _ -> ()
           
let event_loop app _ =
  let e = Sdl.Event.create () in 
  let e_some = Some e in
  let tick_period = tick_period app.config.tick_hz in
  let rec loop next = 
    let now = tick_now () in 
    let next = 
      if now < next then next else begin 
        callback_ev app (`Tick (elapsed_of_ticks now));
        tick_next now tick_period
      end
    in
    if Sdl.poll_event e_some then do_event app e else
    begin
      let dur = min (dur_to_next_ms now next) 10L in
      Sdl.delay (Int64.to_int32 dur);      
    end;
    if app.state = `Ok then loop next else app.state
  in
  loop (tick_next (tick_now ()) tick_period)

let create config = 
  if config.tick_hz < 0 then 
    invalid_arg (str "tick_hz must be positive (%d)" config.tick_hz);
  let app = 
    { log = Sdl.log; 
      config;
      text_ev = false; 
      is_fullscreen = false;
      init = (Sdl.init Sdl.Init.video >>= fun () -> create_window config);
      state = `Ok
    }
  in
  match app.init with 
  | `Ok _ -> 
      callback_ev app (`Env `Init);
      callback_ev app (`Env (`Resize app.config.size));
      app
  | _ -> app

let update_surface app = match app.init with 
| `Ok (win, _) -> Sdl.gl_swap_window win
| _ -> ()

let run app = match app.init with 
| `Error m -> `Error m 
| `Ok (win, ctx) -> 
    match app.state with
    | `Yield | `Ok -> 
        begin match event_loop app win with 
        | `Error m -> `Error m
        | `Yield -> `Ok
        | `Ok -> assert false 
        | `Quit -> 
            ignore (app.config.ev app (`Env `Exit)); 
            match begin destroy_window win ctx >>= fun () -> 
            Sdl.quit ();
            `Quit end with 
            | `Error m -> `Error m 
            | `Ok () -> `Ok 
            | `Quit -> `Quit
        end
    | `Quit -> `Quit
    | `Error _ as e -> e

let handle_run app = match run app with
| `Ok -> Pervasives.exit 0
| `Error msg -> app.log "%s@." msg; Pervasives.exit 1
| `Quit -> Pervasives.exit 0


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
