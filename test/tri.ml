(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Draws a tri-colored triangle. *) 

open Gg
open Lit

let triangle () =
  let b = Buf.create (`Cpu (`Float32, 3 * 3 + 3 * 4)) in 
  let vertices = Attr.create Attr.vertex ~dim:3 b in 
  let colors = Attr.create Attr.color ~dim:4 ~first:(3 * 3) b in
  let prim = Prim.create ~count:3 `Triangles [vertices; colors] in
  let b = Buf.get_cpu b Ba.Float32 in
  Ba.set_3d b 0  (-0.8) (-0.8) ( 0.0);  (* vertices *) 
  Ba.set_3d b 3  ( 0.8) (-0.8) ( 0.0);
  Ba.set_3d b 6  ( 0.0) ( 0.8) ( 0.0);
  Ba.set_v4 b 9  Color.red;             (* colors *) 
  Ba.set_v4 b 13 Color.green;
  Ba.set_v4 b 17 Color.blue;
  prim
    
let program = 
  Prog.create [
    Prog.shader `Vertex "
    in vec3 vertex;
    in vec4 color; 
    out vec4 v_color; 
    void main() 
    { 
        v_color = color; 
        gl_Position = vec4(vertex, 1.0); 
    }"; 

    Prog.shader `Fragment "
      in vec4 v_color; 
      out vec4 color; 
      void main () { color = v_color; }"
  ]

let effect = Effect.create program 

let op = Renderer.op effect (triangle ())

(* Render *) 

let size = V2.v 600. 400.
let r = Renderer.create ~size (App.select_backend ()) 

let view = View.create () 
let draw () = 
  Renderer.set_view r view;
  Renderer.add_op r op;
  Renderer.render r

let ev app ev = match (ev : App.ev) with 
| `Env `Exit -> Renderer.release r; `Ok
| `Env `Init -> 
    Format.printf "@[%a@]@." Renderer.Cap.pp_gl_synopsis r; `Ok
| `Env (`Resize size) ->
    Renderer.set_size r size; 
    draw ();
    App.update_surface app;
    `Ok
| `Tick _ -> assert false
| _ -> `Ok 

let app = App.create { App.default with App.size = size; ev }
let () = App.handle_run app

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
