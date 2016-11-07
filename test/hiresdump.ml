(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(* Litu's wireframe shader. *)

open Gg
open Lit

(* TODO change that when we have multipass *)

let back = Litu.Effect.Wireframe.create
    ~raster:{ Effect.raster_default with
              Effect.raster_face_cull = Some `Front; }
    ~wire_color:(Color.gray 0.8)
    ~fill_color:(Color.gray 0.95)
    ~wire_width: 2.0
    ()

let front = Litu.Effect.Wireframe.create
    ~raster:{ Effect.raster_default with
              Effect.raster_face_cull = Some `Back; }
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
  let size = V2.(4. * Dapp.surface_size app) in
  let mfb = msample_fb r size in
  let cfb = color_fb r size in
  mfb, cfb, size

let hi_dump r app =
  let aspect = Size2.aspect (Dapp.surface_size app) in
  let mfb, cfb, size = offline_hiq_fb r app aspect in
  let box = Box2.v P2.o size in
  let fmt = Raster.Sample.(format rgba_l `UInt8) in
  let scalar_count = Raster.Sample.scalar_count (`D2 (Box2.size box)) fmt in
  let buf = Buf.create (`Gpu (`UInt8, scalar_count)) in
  let saved_fb = Renderer.fbuf r in
  let restore () =
    Renderer.set_fbuf r saved_fb;
    resize r (Dapp.surface_size app);
  in
  Renderer.set_fbuf r mfb;
  resize r size;
  draw r;
  Dapp.update_surface app;
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

let draw r app = draw r; Dapp.update_surface app

let rec command r app = function
| `Init -> resize r (Dapp.surface_size app); `Ok
| `Resize size -> resize r size; draw r app; `Ok
| `Exit -> Renderer.release r; `Quit
| `Toggle_fullscreen -> Dapp.toggle_fullscreen app; `Ok
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
    resize r (Dapp.surface_size app); draw r app; `Ok
| `Move_out ->
    let fwd = V3.(unit (of_v4 (M4.col 3 (View.tr (Renderer.view r))))) in
    from := V3.(!from - 0.2 * fwd);
    resize r (Dapp.surface_size app); draw r app; `Ok
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
  let app = Dapp.create { Dapp.default with Dapp.size = size;
                                          hidpi = true;
                                          tick_hz = 0; } in
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
