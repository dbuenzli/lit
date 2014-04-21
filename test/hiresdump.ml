(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Litu's wireframe shader. *)

open Gg
open Lit

(* TODO change that when we have multipass *) 

let back = Litu.Effect.Wireframe.create 
    ~raster:{ Effect.raster_face_cull = Some `Front }
    ~wire_color:(Color.gray 0.8)
    ~fill_color:(Color.gray 0.95)
    ~wire_width: 2.0
    ()
  
let front = Litu.Effect.Wireframe.create 
    ~raster:{ Effect.raster_face_cull = Some `Back }
    ~depth:{ Effect.depth_default with Effect.depth_test = Some `Lequal } 
    ~wire_color:Color.black
    ~wire_only:true
    ~wire_width:6.0
    ()
              
(* World *) 

let next_prim = Demo.prim_cycler () 
let prim = ref (next_prim ()) 
let prim_tr =
  let angle = Float.pi_div_4 in
  ref (Quat.rot3_zyx (V3.v angle angle 0.))

let rot = ref None

(* Render *) 

let draw r =
  (* Render back faces *)
  let op = Renderer.op back ~tr:(M4.of_quat !prim_tr) !prim in
  Renderer.add_op r op;
  Renderer.render ~clear:true r; 
  (* Render front faces *) 
  let op = { op with effect = front } in 
  Renderer.add_op r op; 
  Renderer.render ~clear:false r


let from = ref (P3.v 0. 0. 5.0)
let resize r size =
  let clears = { Fbuf.clears_default with 
                 Fbuf.clear_color = Some Color.white }
  in
  let aspect = Size2.aspect size in
  let view = 
    let tr = View.look ~at:P3.o ~from:!from () in 
    let fov = `H Float.pi_div_4 in 
    let proj = View.persp ~fov ~aspect ~near:0.1 ~far:10. in
    View.create ~tr ~proj ()
  in
  Format.printf "Setting size: %a@." V2.pp size;
  Renderer.set_size r size;
  Renderer.set_view r view;
  Fbuf.set_clears (Renderer.fbuf r) clears;
  ()

let msample_fb r size = 
  let scount = Renderer.Cap.max_samples r in
  let scount = 0 in
  let color = Fbuf.Rbuf.create ~multisample:scount size (`D4 (`UInt8, true))in 
  let depth = Fbuf.Rbuf.create ~multisample:scount size (`Depth `UInt24) in
  let fb = Fbuf.create [`Color (0, `Rbuf color); `Depth (`Rbuf depth)] in
  let status = Fbuf.status r fb in
  assert (status = `Complete);
  fb

let color_fb r size = 
  let color = Fbuf.Rbuf.create size (`D4 (`UInt8, true))in 
  let fb = Fbuf.create [`Color (0, `Rbuf color); ] in
  let status = Fbuf.status r fb in
  assert (status = `Complete) ;
  fb 
  
let offline_hiq_fb r app aspect = 
  let size =
    let max = float (Renderer.Cap.max_render_buffer_size r / 2) in 
    if aspect > 1. 
    then Size2.v max (Float.round (max /. aspect))
    else Size2.v (Float.round (max *. aspect)) max 
  in
  let size = V2.(4. * App.surface_size app) in
  let mfb = msample_fb r size in 
  let cfb = color_fb r size in
  mfb, cfb, size

let hi_dump r app =
  let aspect = Size2.aspect (App.surface_size app) in
  let mfb, cfb, size = offline_hiq_fb r app aspect in
  let box = Box2.v P2.o size in
  let w = Float.int_of_round (Box2.w box) in 
  let h = Float.int_of_round (Box2.h box) in
  let fmt = Raster.Sample.(format rgba_l `UInt8) in
  let scalar_count = Raster.Sample.scalar_count ~w ~h fmt in
  let buf = Buf.create (`Gpu (`UInt8, scalar_count)) in
  let saved_fb = Renderer.fbuf r in
  let restore () =
    Renderer.set_fbuf r saved_fb;
    resize r (App.surface_size app);
  in
  Renderer.set_fbuf r mfb; 
  resize r size;
  draw r;
  App.update_surface app;
  restore ();
  Fbuf.blit r [`Color] ~src:mfb box ~dst:cfb box; 
  Fbuf.read r cfb (`Color_rgba 0) box buf;
  let ba = Buf.gpu_map r `R buf Ba.UInt8 in
  let fname = "/tmp/out.tga" in
  begin match Tga.write fname `Color_rgba (Box2.size box) ba with 
  | `Ok () -> () | `Error e -> Printf.eprintf "%s: %s" fname e
  end;
  Buf.gpu_unmap r buf;
  `Ok 

let draw r app = draw r; App.update_surface app

let rec command r app = function 
| `Init -> resize r (App.surface_size app); `Ok 
| `Resize size -> resize r size; draw r app; `Ok
| `Exit -> Renderer.release r; `Quit
| `Toggle_fullscreen -> App.toggle_fullscreen app; `Ok 
| `Rot_start pt ->
    let pt = View.ndc_of_surface (Renderer.view r) pt in 
    rot := Some (Litu.Manip.rot ~init:!prim_tr ~start:pt ()); `Ok
| `Rot_end _ -> 
    rot := None; `Ok
| `Rot_update pt ->
    begin match !rot with 
    | None -> `Ok 
    | Some rot -> 
        let pt = View.ndc_of_surface (Renderer.view r) pt in
        prim_tr := Litu.Manip.rot_update rot pt; draw r app; `Ok
    end
| `Move_in -> 
    let fwd = V3.(unit (of_v4 (M4.col 3 (View.tr (Renderer.view r))))) in 
    from := V3.(!from + 0.2 * fwd);
    resize r (App.surface_size app); draw r app; `Ok
| `Move_out -> 
    let fwd = V3.(unit (of_v4 (M4.col 3 (View.tr (Renderer.view r))))) in 
    from := V3.(!from - 0.2 * fwd);
    resize r (App.surface_size app); draw r app; `Ok
| `Cycle_prim -> 
    prim := next_prim (); draw r app; `Ok
| `None e -> 
    begin match e with 
    | `Mouse (`Button (`Down, `Left, pt)) -> command r app (`Rot_start pt)
    | `Mouse (`Button (`Up, `Left, pt)) -> command r app (`Rot_end pt)
    | `Mouse (`Motion (pt, _)) -> command r app (`Rot_update pt)
    | `Key (`Down, (`Uchar 0x0064)) -> 
        Format.printf "dumping@.";
        hi_dump r app
    | _ -> `Ok
    end
| _ -> `Ok

let main () = 
  let size = Demo.default_size in
  let app = App.create { App.default with App.size = size; 
                                          hidpi = true;
                                          tick_hz = 0; } in
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
