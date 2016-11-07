(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Checkboard texture on a square. *)

open Gg
open Lit

let img_tex () = 
  let img = 
    let img = Ba.create Bigarray.int8_unsigned (64 * 64 * 3) in 
    let i = ref 0 in
    for y = 0 to 63 do
      for x = 0 to 63 do
        let xm = if x land 8 = 0 then 1 else 0 in 
        let ym = if y land 8 = 0 then 1 else 0 in 
        let l = (xm lxor ym) * 225 in
        i := Ba.set_3d img !i l l l;
      done
    done; 
    Buf.create (`Bigarray img)
  in
  Tex.create 
    ~wrap_s:`Clamp_to_edge ~wrap_t:`Clamp_to_edge 
    ~mipmaps:true 
    ~min_filter:`Nearest
    ~mag_filter:`Nearest
    ~format:`RGB_UInt8_norm (`D2 (Size2.v 64. 64., Some img))

let program = 
  let img = Uniform.tex "img" (img_tex ()) in
  let uset = Uniform.(empty + model_to_clip "model_to_clip" + img) in
  Prog.create ~uset [
    Prog.shader `Vertex "
     uniform mat4 model_to_clip;
     in vec3 vertex;
     in vec2 tex; 
     out vec2 v_tex;
     void main () 
     {
        v_tex = tex;
        gl_Position = model_to_clip * vec4(vertex, 1.0);
     }";

    Prog.shader `Fragment "
    uniform sampler2D img;
    in vec2 v_tex;
    out vec4 f_color;
    void main () 
    {
      f_color = texture(img, v_tex);
    }"
  ]

let effect = Effect.create program 

(* World *) 

let rect size =
  let attrs = 
    let w = Size2.w size in 
    let h = Size2.h size in
    let b = Ba.create Bigarray.float32 (4 * 2 + 4 * 2) in
    let i = Ba.set_2d b 0 (0.0) (0.0) in  (* vertices *) 
    let i = Ba.set_2d b i (  w) (0.0) in
    let i = Ba.set_2d b i (  w) (  h) in
    let i = Ba.set_2d b i (0.0) (  h) in
    let i = Ba.set_2d b i (0.0) (0.0) in  (* tex coords *) 
    let i = Ba.set_2d b i (1.0) (0.0) in
    let i = Ba.set_2d b i (1.0) (1.0) in
    let _ = Ba.set_2d b i (0.0) (1.0) in
    let b = Buf.create (`Bigarray b) in
    [ Attr.create Attr.vertex ~dim:2 b; 
      Attr.create "tex" ~dim:2 ~first:(4 * 2) b ]
  in
  let index =                                            
    let b = Ba.create Bigarray.int32 (2 * 3) in 
    let i = Ba.seti_3d b 0 0 2 3 in                           (* triangles *) 
    let _ = Ba.seti_3d b i 0 1 2 in
    Buf.create ~unsigned:true (`Bigarray b) 
  in
  Prim.create ~index `Triangles attrs

let prim = rect (Size2.v 64. 64.)

(* Render *) 

let draw r = 
  let op = { count = 1; tr = M4.move (V3.v 0. 0. 0.); effect; prim } in
  Renderer.add_op r op;
  Renderer.render r

let resize r size =
  let clears = { Renderer.default_clears with 
                 Renderer.clear_color = Some Color.white }
  in
  let w = Size2.w size in 
  let h = Size2.h size in
  let proj = M4.ortho ~left:0. ~right:w ~bottom:0. ~top:h ~near:0. ~far:1. in
  let view = View.create  ~proj () in
  Renderer.set_size r size;
  Renderer.set_view r view;  
  Renderer.set_clears r clears;
  ()

let draw r app = draw r; App.update_surface app

(* Ui *) 

let rec command r app = function 
| `Init -> resize r (App.size app); `Ok 
| `Resize size -> resize r size; draw r app; `Ok
| `Toggle_fullscreen -> App.toggle_fullscreen app; `Ok 
| `Exit -> Renderer.release r; `Quit
| _ -> `Ok 

let main () = 
  let size = Demo.default_size in
  let r = Renderer.create ~size (App.select_backend ()) in
  let ev = Demo.ev_of_command_handler (command r) in
  let app = App.create { App.default with App.size = size; tick_hz = 0; ev } in
  App.handle_run app 
  
let () = main ()

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
