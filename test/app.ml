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

type key = [ `Down | `Up ] * keysym

type mouse_button = [ `Left | `Right | `Middle | `X1 | `X2 ]
type mouse = 
  [ `Button of [ `Down | `Up ] * mouse_button * p2 
  | `Motion of p2 * v2 ]

type ev = 
  [ `Env of env 
  | `Tick of float 
  | `Key of key 
  | `Mouse of mouse 
  | `Text of string ]

type ev_ret = [ `Ok | `Error of string | `Yield | `Quit ]

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

let size app = match app.init with 
| `Ok (win, _) -> 
    let w, h = Sdl.get_window_size win in
    Size2.v (float w) (float h)
| _ -> app.config.size 


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
  set Sdl.Gl.red_size 8                                       >>= fun () ->
  set Sdl.Gl.green_size 8                                     >>= fun () ->
  set Sdl.Gl.blue_size 8                                      >>= fun () ->
  set Sdl.Gl.alpha_size 8                                     >>= fun () ->
  set Sdl.Gl.depth_size 24                                    >>= fun () ->
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

let text_editing_ev app e = 
  let text = Sdl.Event.(get e text_editing_text) in 
  let start = Sdl.Event.(get e text_editing_start) in 
  let len = Sdl.Event.(get e text_editing_length) in 
  Printf.printf "TODO Text editing ! t:%s start:%d len:%d" text start len
    
let text_input_ev app e = 
  let text = Sdl.Event.(get e text_input_text) in
  callback_ev app (`Text text)


let keysym_to_string (ksym : keysym) = 
  let dir_to_string = function 
  | `Left -> "left" | `Right -> "right" | `Up -> "up" | `Down -> "down"
  in
  match ksym with
  | `Alt dir -> str "alt_%s" (dir_to_string dir)
  | `Arrow dir -> str "arrow_%s" (dir_to_string dir)
  | `Backspace -> "backspace"
  | `Ctrl dir -> str "ctrl_%s" (dir_to_string dir)
  | `Digit d -> str "digit_%d" d
  | `End -> "end"
  | `Enter -> "enter"
  | `Escape -> "escape" 
  | `Function n -> str "f%d" n
  | `Home -> "home" 
  | `Meta dir -> str "meta_%s" (dir_to_string dir)
  | `Page dir -> str "page_%s" (dir_to_string dir)
  | `Return -> "return"
  | `Shift dir -> str "shift_%s" (dir_to_string dir)
  | `Space -> "space"
  | `Tab -> "tab" 
  | `Uchar u -> str "U+%04X" u
  | `Unknown u -> str "unknown (%X)" u 

let pp_keysym ppf ksym = Format.fprintf ppf "%s" (keysym_to_string ksym)

module Int = struct 
  type t = int 
  let compare : int -> int -> int = Pervasives.compare 
end

let keysym_of_keycode = 
  let module Imap = Map.Make (Int) in
  let map = [
    Sdl.K.lalt, `Alt `Left; Sdl.K.ralt, `Alt `Right;
    Sdl.K.up, `Arrow `Up; Sdl.K.down, `Arrow `Down; 
    Sdl.K.left, `Arrow `Left; Sdl.K.right, `Arrow `Right;
    Sdl.K.backspace, `Backspace; 
    Sdl.K.lctrl, `Ctrl `Left; Sdl.K.rctrl, `Ctrl `Right; 
    Sdl.K.k0, `Digit 0; Sdl.K.k1, `Digit 1; Sdl.K.k2, `Digit 2;
    Sdl.K.k3, `Digit 3; Sdl.K.k4, `Digit 4; Sdl.K.k5, `Digit 5;
    Sdl.K.k6, `Digit 6; Sdl.K.k7, `Digit 7; Sdl.K.k8, `Digit 8;
    Sdl.K.k9, `Digit 9;
    Sdl.K.kp_0, `Digit 0; Sdl.K.kp_1, `Digit 1; Sdl.K.kp_2, `Digit 2;
    Sdl.K.kp_3, `Digit 3; Sdl.K.kp_4, `Digit 4; Sdl.K.kp_5, `Digit 5;
    Sdl.K.kp_6, `Digit 6; Sdl.K.kp_7, `Digit 7; Sdl.K.kp_8, `Digit 8;
    Sdl.K.kp_9, `Digit 9;
    Sdl.K.kend, `End;
    Sdl.K.kp_enter, `Enter;
    Sdl.K.escape, `Escape;
    Sdl.K.f1, `Function 1; Sdl.K.f2, `Function 2; Sdl.K.f3, `Function 3; 
    Sdl.K.f4, `Function 4; Sdl.K.f5, `Function 5; Sdl.K.f6, `Function 6; 
    Sdl.K.f7, `Function 7; Sdl.K.f8, `Function 8; Sdl.K.f9, `Function 9; 
    Sdl.K.f10, `Function 10; Sdl.K.f11, `Function 11; Sdl.K.f12, `Function 12; 
    Sdl.K.f13, `Function 13; Sdl.K.f14, `Function 14; Sdl.K.f15, `Function 15; 
    Sdl.K.f16, `Function 16; Sdl.K.f17, `Function 17; Sdl.K.f18, `Function 18; 
    Sdl.K.f19, `Function 19; Sdl.K.f20, `Function 20; Sdl.K.f21, `Function 21; 
    Sdl.K.f22, `Function 22; Sdl.K.f23, `Function 23; Sdl.K.f24, `Function 24; 
    Sdl.K.home, `Home;
    Sdl.K.lgui, `Meta `Left; Sdl.K.rgui, `Meta `Right; 
    Sdl.K.pagedown, `Page `Down; 
    Sdl.K.pageup, `Page `Up; 
    Sdl.K.return, `Return; 
    Sdl.K.lshift, `Shift `Left; Sdl.K.rshift, `Shift `Right; 
    Sdl.K.space, `Space; 
    Sdl.K.tab, `Tab; ]
  in 
  let m = List.fold_left (fun acc (k, v) -> Imap.add k v acc) Imap.empty map in
  fun kc -> try Imap.find kc m with 
  | Not_found -> 
      if kc land Sdl.K.scancode_mask > 0 then `Unknown kc else `Uchar kc
      
let keyboard_ev app e state =
  let keysym = keysym_of_keycode (Sdl.Event.(get e keyboard_keycode)) in
  callback_ev app (`Key (state, keysym))

let window_ev app e = 
  match Sdl.Event.(window_event_enum (get e window_event_id)) with
  | `Exposed | `Resized -> callback_ev app (`Env (`Resize (size app)))
  | _ -> ()

let mouse_button_ev app e state =   
  let pos =
    let x = Sdl.Event.(get e mouse_button_x) in 
    let y = Sdl.Event.(get e mouse_button_y) in
    let size = size app in
    P2.v (float x /. Size2.w size) (1. -. (float y /. Size2.h size))      
  in
  let but = match Sdl.Event.(get e mouse_button_button) with 
  | b when b = Sdl.Button.left -> `Left
  | b when b = Sdl.Button.right -> `Right
  | b when b = Sdl.Button.middle -> `Middle 
  | b when b = Sdl.Button.x1 -> `X1
  | b when b = Sdl.Button.x2 -> `X2
  | b -> `Left (* avoid assert false, SDL may come up with new things *) 
  in
  callback_ev app (`Mouse (`Button (state, but, pos)))

let mouse_motion_ev app e = 
  let size = size app in
  let pos = 
    let x = Sdl.Event.(get e mouse_motion_x) in 
    let y = Sdl.Event.(get e mouse_motion_y) in 
    P2.v (float x /. Size2.w size) (1. -. (float y /. Size2.h size))
  in
  let rel = 
    let xrel = Sdl.Event.(get e mouse_motion_xrel) in 
    let yrel = Sdl.Event.(get e mouse_motion_yrel) in
    P2.v (float xrel /. Size2.w size) (1. -. (float yrel /. Size2.h size))
  in
  callback_ev app (`Mouse (`Motion (pos, rel)))

let do_event app e = 
  let event e = Sdl.Event.(enum (get e typ)) in
  match event e with 
  | `Quit -> app.state <- `Quit
  | `Key_down -> keyboard_ev app e `Down 
  | `Key_up -> keyboard_ev app e `Up
  | `Window_event -> window_ev app e 
  | `Mouse_button_down -> mouse_button_ev app e `Down
  | `Mouse_button_up -> mouse_button_ev app e `Up
  | `Mouse_motion -> mouse_motion_ev app e
  | `Text_editing -> text_editing_ev app e
  | `Text_input -> ignore (text_input_ev app e)
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
