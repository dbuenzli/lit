(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Diffuse ligthing shader. *)

open Gg
open Lit

let fullscreen () = (* two triangles covering the projection of clip space *) 
  let attrs =                                    
    let b = Ba.create Ba.Float32 (4 * 3) in 
    Ba.set_3d b 0 ( 1.) ( 1.) ( 0.);                 (* vertices *)
    Ba.set_3d b 3 (-1.) ( 1.) ( 0.);
    Ba.set_3d b 6 ( 1.) (-1.) ( 0.);
    Ba.set_3d b 9 (-1.) (-1.) ( 0.);
    let b = Buf.create (`Float32 b) in
    [ Attr.create Attr.vertex ~dim:3 b ]
  in 
  let index =                                            
    let b = Ba.create Ba.UInt8 (2 * 3) in 
    Ba.set_3d b 0 0 1 2;                           (* triangles *)
    Ba.set_3d b 3 2 1 3;
    Buf.create (`UInt8 b) 
  in
  Prim.create ~index `Triangles attrs


let light_pos = Uniform.v3 "light_pos" (V3.v 3. 5. 5.)
let light_color = Uniform.v3 "light_color" (V3.v 1. 1. 1.)
let kd = Uniform.v3 "kd" (V3.v 0.5 0.5 0.5)
let effect = 
  let uset = Uniform.(empty + 
                      model_to_view "model_to_view" + 
                      model_normal_to_view "normal_to_view" +
                      model_to_clip "model_to_clip" +
                      world_to_view "world_to_view" + 
                      light_pos + light_color + kd)
  in
  let program = Prog.create ~uset [
    Prog.shader `Vertex "
    uniform mat4 model_to_view; 
    uniform mat3 normal_to_view; 
    uniform mat4 model_to_clip;

    in vec3 vertex;
    in vec3 normal; 
    out vec4 v_position;
    out vec3 v_normal;
    void main () 
    {
       v_position = model_to_view * vec4(vertex, 1.0);
       v_normal = normal_to_view * normal; 
       gl_Position = model_to_clip * vec4(vertex, 1.0);
    }";

    Prog.shader `Fragment "
    uniform mat4 world_to_view;
    uniform vec3 light_pos;
    uniform vec3 light_color; 
    uniform vec3 kd;

    in vec4 v_position; 
    in vec3 v_normal; 
    out vec4 f_color;
    void main () 
    {
      vec4 f_light_pos = world_to_view * vec4(light_pos, 1.0);
      vec4 l = normalize(f_light_pos - v_position);
      float I = clamp(dot(normalize(v_normal), l.xyz), 0, 1);
      f_color = vec4(clamp(kd * light_color * I, 0, 1), 1.0);
    }"
  ]
  in
  Effect.create program 

let time = Uniform.float "time" 0.
let fullscreen_effect = 
  let uset = Uniform.(empty + viewport_size "view_size" + time) in
  let prog = Prog.create ~uset [
    Prog.shader `Vertex "
    in vec3 vertex;
    void main() { gl_Position = vec4(vertex, 1.0); }";
    Prog.shader `Fragment "
    uniform sampler2DMS fb; 
    uniform vec2 view_size;
    uniform float time;
    out vec4 color;

    vec4 msample (sampler2DMS sampler, ivec2 p, int scount)
    {
      vec4 acc = vec4(0);
      for (int s = 0; s < scount; s++) 
      {
         acc += texelFetch(sampler, p, s);
      }
      return acc / scount; 
    }
    const vec2 center = vec2(400, 300); 
    const float r = 200; 
    const float angle = 0.8;
    vec4 swirl (sampler2DMS sampler, vec2 p, int scount)
    {
      p -= center; 
      float d = length (p); 
      if (d < r) 
      { 
         float ratio = (r - d) / r; 
         float theta = ratio * ratio * angle * 8.0; 
         float s = sin(theta); 
         float c = cos(theta); 
         p = vec2(dot(p, vec2(c, -s)), dot(p, vec2(s, c)));
      }
      p += center;
      return msample(sampler, ivec2(p), 8);
    }
    void main() 
    {
      vec2 p = gl_FragCoord.xy;
      vec2 delta = vec2(cos(300 * p.x + time * 2), sin(400 * p.y + time * 2)); 
      delta *= vec2(3,3) * delta;
      
      color = vec4(msample(fb, ivec2(p) + ivec2(delta), 8));
      color = swirl(fb, p, 8); 

/*      vec4 acc = vec4(0); 
      for (int x = -4; x <= 4; x++)
       for (int y = -4; y <= 4; y++) 
        acc += msample(fb, ivec2(p) + ivec2(x,y), 8);                    
      color = vec4(acc.rgb / 81, 1);
*/
    }" ]
  in
  Effect.create prog


(* World *) 

let next_prim = Demo.prim_cycler ~normals:true () 
let prim = ref (next_prim ()) 
let prim_tr =
  let angle = Float.pi_div_4 in
  ref (Quat.rot3_zyx (V3.v angle angle 0.))

let rot = ref None

let fullscreen = fullscreen () 

(* Render *) 

let fs_uniforms = ref Uniform.empty
let fbuf = ref Fbuf.default

let draw r = 
  let op = Renderer.op effect ~tr:(M4.of_quat !prim_tr) !prim in
  Renderer.set_fbuf r !fbuf;
  assert(Renderer.Fbuf.complete r !fbuf = `Complete);
  Renderer.add_op r op;
  Renderer.render r; 
  let op = Renderer.op ~uniforms:!fs_uniforms fullscreen_effect fullscreen in
  Renderer.set_fbuf r Fbuf.default; 
  Renderer.add_op r op; 
  Renderer.render r
  

let resize r size =
  let clears = { Renderer.clears_default with 
                 Renderer.clear_color = Some Color.white }
  in
  let aspect = Size2.w size /. Size2.h size in
  let view = 
    let tr = View.look ~at:P3.o ~from:(P3.v 0. 0. 5.) () in 
    let fov = `H Float.pi_div_4 in 
    let proj = View.persp ~fov ~aspect ~near:1.0 ~far:10. in
    View.create ~tr ~proj ()
  in
  let new_fb, color = 
    let scount = Renderer.Cap.max_samples r in
    let color = 
      Tex.create ~mag_filter:`Linear ~min_filter:`Linear 
        (`D2_ms (`D3 (`UInt8, true), size, scount, true)) 
    in 
    let depth = Fbuf.Rbuf.create ~multisample:scount size (`Depth `UInt24) in
    let fbuf = Fbuf.create [`Color (0, `Tex (0, color)); `Depth (`Rbuf depth)]in
    assert(Renderer.Fbuf.complete r fbuf = `Complete); 
    fbuf, color
  in
  fbuf := new_fb;
  fs_uniforms := Uniform.(!fs_uniforms + tex "fb" color);
  Renderer.set_size r size;
  Renderer.set_view r view;  
  Renderer.set_clears r clears;
  ()

let draw r app = draw r; App.update_surface app

let rec command r app = function 
| `Init -> resize r (App.size app); `Ok 
    
| `Resize size -> resize r size; draw r app; `Ok
| `Exit -> Renderer.release r; `Quit
| `Toggle_fullscreen -> App.toggle_fullscreen app; `Ok 
| `Rot_start pt -> 
    let pt = View.ndc_of_surface (Renderer.view r) pt in 
    rot := Some (Litu.Manip.rot ~init:!prim_tr ~start:pt ()); `Ok
| `Rot_end _ -> rot := None; `Ok
| `Rot_update pt ->
    begin match !rot with 
    | None -> `Ok 
    | Some rot ->
        let pt = View.ndc_of_surface (Renderer.view r) pt in
        prim_tr := Litu.Manip.rot_update rot pt; draw r app; `Ok
    end
| `Cycle_prim -> 
    prim := next_prim (); draw r app; `Ok
| `None e ->
    begin match e with 
    | `Mouse (`Button (`Down, `Left, pt)) -> command r app (`Rot_start pt)
    | `Mouse (`Button (`Up, `Left, pt)) -> command r app (`Rot_end pt)
    | `Mouse (`Motion (pt, _)) -> command r app (`Rot_update pt)
    | _ -> `Ok
    end
| `Tick now -> 
    fs_uniforms := Uniform.(!fs_uniforms + (Uniform.set_value time now)); 
    draw r app; 
    `Ok
| _ -> `Ok 


let main () = 
  let size = Demo.default_size in
  let app = App.create { App.default with App.size = size; tick_hz = 60; 
                                          hidpi = false; } in
  let r = Renderer.create ~size (App.select_backend ()) in
  let ev = Demo.ev_of_command_handler (command r) in
  App.handle_run app ~ev
  
let () = main ()

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

