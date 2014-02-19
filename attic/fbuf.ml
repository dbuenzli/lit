(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

type t = {
    mutable width : int; 
    mutable height : int;
    (* stats *)
    mutable frame_stamp : float;                  (* start time of last frame *)
    mutable sample_stamp : float;                     (* start time of sample *)
    mutable sample_frame_count : int;           (* number of frames in sample *)
    mutable frame_hz : float; 
    mutable max_frame_hz : float;
    mutable min_frame_hz : float;
    mutable frame_time : float;                            (* last frame time *)
    mutable max_frame_time : float;
    mutable min_frame_time : float;
    mutable frame_vertex_count : int; 
    mutable frame_face_count : int;   }

let nil_time = ~-.1.0
let max_time = max_float
let min_time = min_float

let _create ~w ~h = { 
  width = w; 
  height = h; 
  frame_stamp = nil_time;
  sample_stamp = nil_time;
  sample_frame_count = 0;
  frame_hz = 0.0; 
  max_frame_hz = min_float; 
  min_frame_hz = max_float;
  frame_time = 0.0; 
  max_frame_time = min_time; 
  min_frame_time = max_time;
  frame_vertex_count = 0;
  frame_face_count = 0; }
    

(*
let contents ?(format = (Pixel.RGBA Pixel.PL_8_8_8_8)) d = 
  failwith "TODO unimplemented" 
let size d = d.width, d.height
let set_size d ~w ~h = d.width <- w; d.height <- h

let default_drawable r = r.default_drawable 
let drawable r = r.drawable
let set_drawable r d = r.drawable <- d 
*)

external time_now : unit -> float = "lit_time_now"

module Stats = struct
  let reset d = 
    d.frame_stamp <- nil_time;
    d.sample_stamp <- nil_time;
    d.sample_frame_count <- 0;
    d.frame_hz <- 0.0; 
    d.max_frame_hz <- min_float;
    d.min_frame_hz <- max_float;
    d.frame_time <- 0.0; 
    d.max_frame_time <- min_time; 
    d.min_frame_time <- max_time;
    d.frame_vertex_count <- 0;
    d.frame_face_count <- 0
	
  let update d = 
    let now = time_now () in
    d.frame_vertex_count <- 0; 
    d.frame_face_count   <- 0;
    if (d.frame_stamp) != nil_time then
      begin
	let sample_time = now -. d.sample_stamp in 
	d.sample_frame_count <- d.sample_frame_count + 1;
	if sample_time >= 1.0 then
	  begin
	    d.frame_hz <- (float_of_int d.sample_frame_count) /. sample_time;
	    if d.frame_hz > d.max_frame_hz then d.max_frame_hz <- d.frame_hz;
	    if d.frame_hz < d.min_frame_hz then d.min_frame_hz <- d.frame_hz;
	    d.sample_frame_count <- 0;
	    d.sample_stamp <- now;
	  end;
	d.frame_time <- now -. d.frame_stamp;
	if d.frame_time > d.max_frame_time then 
	  d.max_frame_time <- d.frame_time;
	if d.frame_time < d.min_frame_time then
	  d.min_frame_time <- d.frame_time;
	d.frame_stamp <- now;
      end
    else	
      begin
	d.frame_stamp <- now;
	d.sample_stamp <- now;
      end
      
  let frame_hz d = d.frame_hz
  let max_frame_hz d = d.max_frame_hz
  let min_frame_hz d = d.min_frame_hz
  let frame_time d = d.frame_time
  let max_frame_time d = d.max_frame_time
  let min_frame_time d = d.min_frame_time
  let frame_vertex_count d = d.frame_vertex_count
  let frame_face_count d = d.frame_face_count

  let add_geometry d ~vertices ~faces = 
    d.frame_vertex_count <- d.frame_vertex_count + vertices;
    d.frame_face_count <- d.frame_face_count + faces
end



