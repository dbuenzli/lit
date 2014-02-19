(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Animated vortex. 

   Taken here http://mrdoob.github.com/three.js/examples/webgl_shader.html
   Equation due to http://badc0de.jiggawatt.org/ *)

open Gg
open Lit

let pp = Format.printf 

let fullscreen () = (* two triangles covering the projection of clip space *) 
  let attrs =                                    
    let b = Ba.create Bigarray.float32 (4 * 3) in 
    let i = Ba.set_3d b 0 ( 1.) ( 1.) ( 0.) in                 (* vertices *) 
    let i = Ba.set_3d b i (-1.) ( 1.) ( 0.) in
    let i = Ba.set_3d b i ( 1.) (-1.) ( 0.) in
    let _ = Ba.set_3d b i (-1.) (-1.) ( 0.) in
    let b = Buf.create (`Bigarray b) in
    [ Attr.create Attr.vertex ~dim:3 b ]
  in 
  let index =                                            
    let b = Ba.create Bigarray.int32 (2 * 3) in 
    let i = Ba.seti_3d b 0 0 1 2 in                           (* triangles *) 
    let _ = Ba.seti_3d b i 2 1 3 in
    Buf.create ~unsigned:true (`Bigarray b) 
  in
  Prim.create ~index `Triangles attrs

let program = 
  let uset, _ = Uniform.(set empty @@ u "time" Time) in 
  let uset, _ = Uniform.(set uset  @@ u "view_size" Viewport_size) in
  Prog.create ~uset [
    Prog.shader `Vertex "
    in vec3 vertex;
    void main() { gl_Position = vec4(vertex, 1.0); }";

    Prog.shader `Fragment "
    uniform vec2 view_size;
    uniform float time;
    out vec4 color;
    void main() 
    {
      float time = 2 * time;
      vec2 p = -1.0 + 2.0 * gl_FragCoord.xy / view_size.xy;
      float a = time*40.0;
      float d,e,f,g=1.0/40.0,h,i,r,q;
      e=400.0*(p.x*0.5+0.5);
      f=400.0*(p.y*0.5+0.5);
      i=200.0+sin(e*g+a/150.0)*20.0;
      d=200.0+cos(f*g/2.0)*18.0+cos(e*g)*7.0;
      r=sqrt(pow(i-e,2.0)+pow(d-f,2.0));
      q=f/r;
      e=(r*cos(q))-a/2.0;f=(r*sin(q))-a/2.0;
      d=sin(e*g)*176.0+sin(e*g)*164.0+r;
      h=((f+d)+a/2.0)*g;
      i=cos(h+r*p.x/1.3)*(e+e+a)+cos(q*g*6.0)*(r+h/3.0);
      h=sin(f*g)*144.0-sin(e*g)*212.0*p.x;
      h=(h+(f-e)*q+sin(r-(a+h)/7.0)*10.0+i/4.0)*g;
      i+=cos(h*2.3*sin(a/350.0-q))*184.0*sin(q-(r*4.3+a/12.0)*g)+tan(r*g+h)*
         184.0*cos(r*g+h);
      i=mod(i/5.6,256.0)/64.0;
      if(i<0.0) i+=4.0;
      if(i>=2.0) i=4.0-i;
      d=r/350.0;
      d+=sin(d*d*8.0)*0.52;
      f=(sin(a*g)+1.0)/2.0;
      color=vec4(vec3(f*i/1.6,i/2.0+d/13.0,i)*d*p.x+
            vec3(i/1.3+d/8.0,i/2.0+d/18.0,i)*d*(1.0-p.x),1.0);
    }"
  ]


let effect = 
  Effect.create program 
    
let op = { count = 1; effect; prim = fullscreen () } 

(* Render *) 
         
let size = V2.v 600. 400.
let r = Renderer.create ~time:App.elapsed ~size (App.select_backend ()) 

let view = View.create () 
let draw () = 
  Renderer.set_view r view;
  Renderer.frame_begin r; 
  Renderer.frame_add r op;
  Renderer.frame_end r;
  ()
    
let last = ref 0.
let ev app e = match (e : App.ev) with
| `Env `Init -> pp "@[%a@]@." Renderer.Cap.pp_gl_synopsis r; `Ok 
| `Env (`Resize size) -> Renderer.set_size r size; `Ok
| `Env `Exit -> Renderer.release r; `Ok
| `Tick t ->
    let now = App.elapsed () in
    let (), draw = App.time draw () in 
    let (), swap = App.time App.update_surface app in 
    Printf.printf "draw %4.0fdus + swap %4.0fus = %4.0fus dt:%4.0fus  \r%!"
      (draw *. 1e6) (swap *. 1e6) ((swap +. draw) *. 1e6)
      ((now -. !last) *. 1e6);
    last := now;
    `Ok
| _ -> `Ok 

let app = App.create { App.default with App.size = size; tick_hz = 120; ev }
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
