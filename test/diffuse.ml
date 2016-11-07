(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Diffuse ligthing shader. *)

open Gg
open Lit

let light_pos = Uniform.v3 "light_pos" (V3.v 3. 5. 5.)
let light_color = Uniform.v3 "light_color" (V3.v 1. 1. 1.)
let kd = Uniform.v3 "kd" (V3.v 0.5 0.5 0.5)
let program =
  let uset = Uniform.(empty +
                      model_to_view "model_to_view" +
                      model_normal_to_view "normal_to_view" +
                      model_to_clip "model_to_clip" +
                      world_to_view "world_to_view" +
                      light_pos + light_color + kd)
  in
  Prog.create ~uset [
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

let effect = Effect.create program

(* World *)

let next_prim = Demo.prim_cycler ~normals:true ()
let prim = ref (next_prim ())
let prim_tr =
  let angle = Float.pi_div_4 in
  ref (Quat.rot3_zyx (V3.v angle angle 0.))

let rot = ref None

(* Render *)

let draw r =
  let op = Renderer.op effect ~tr:(M4.of_quat !prim_tr) !prim in
  Renderer.add_op r op;
  Renderer.render r

let resize r size =
  let clears = { Fbuf.clears_default with
                 Fbuf.clear_color = Some Color.white }
  in
  let aspect = Size2.w size /. Size2.h size in
  let view =
    let tr = View.look ~at:P3.o ~from:(P3.v 0. 0. 5.) () in
    let fov = `H Float.pi_div_4 in
    let proj = View.persp ~fov ~aspect ~near:1.0 ~far:10. in
    View.create ~tr ~proj ()
  in
  Renderer.set_size r size;
  Renderer.set_view r view;
  Fbuf.set_clears Fbuf.default clears;
  ()

let draw r app = draw r; Dapp.update_surface app

let rec command r app = function
| `Init -> resize r (Dapp.size app); `Ok
| `Resize size -> resize r size; draw r app; `Ok
| `Exit -> Renderer.release r; `Quit
| `Toggle_fullscreen -> Dapp.toggle_fullscreen app; `Ok
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
| _ -> `Ok


let main () =
  let size = Demo.default_size in
  let app = Dapp.create { Dapp.default with Dapp.size = size; tick_hz = 0; } in
  let r = Renderer.create ~size (Dapp.select_backend ()) in
  let ev = Demo.ev_of_command_handler (command r) in
  Dapp.handle_run app ~ev

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
