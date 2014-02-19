(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

open Gg;;

let err_compressed = "compressed image format"
let err_invalid_pf = "invalid pixel format"
    
type dim = [ `D1 | `D2 | `D3 ]
let _string_of_dim = function `D1 -> "`D1" | `D2 -> "`D2" | `D3 -> "`D3"
    
type comps = [ `A | `R | `G | `B | `RGB | `ARGB | `RGBA | `LA | `L | `D ]
let comps_count = function
  | (`A | `R | `G | `B | `L | `D) -> 1
  | `LA -> 2  
  | `RGB -> 3
  | (`ARGB | `RGBA) -> 4 
	
type storage = [ Buf.uint_kind | `Float32]
let _string_of_storage = function
  | `Uint8 -> "`Uint8"
  | `Uint16 -> "`Uint16"
  | `Uint32 -> "`Uint32"
  | `Float32 -> "`Float32"
 
type 'a pf = [ 
  | `A of 'a
  | `R of 'a
  | `G of 'a 
  | `B of 'a
  | `L of 'a
  | `LA of 'a
  | `RGB of 'a * [`C | `P332 | `P565 | `DXT1 ]
  | `RGBA of 'a * [`C | `P4444 | `P5551 | `P8888 | `P1010102 | `DXT1 | 
                   `DXT3 | `DXT5]
  | `ARGB of 'a * [`P4444 | `P1555 | `P8888 | `P2101010 ]]
constraint 'a = [< storage ]
   	
let pf_compressed = function
  | (`RGB (_, `DXT1) | `RGBA (_, (`DXT1 | `DXT3 | `DXT5))) -> true
  | #pf -> false
	
let block_size = function
  | (`RGB (_, `DXT1) | `RGBA (_, `DXT1)) -> 8
  | `RGBA (_ , (`DXT3 | `DXT5)) -> 16
  | #pf -> assert false

let pf_storage = function
  | (`A s | `R s | `G s | `B s | `L s |
     `LA s | `RGB (s, _) | `RGBA (s, _) | `ARGB (s, _)) -> s

let pf_cast s pf = Check.spec _string_of_storage (pf_storage pf) s; pf    

let pf_comps = function
  | `A _ -> `A 
  | `R _ -> `R 
  | `G _ -> `G 
  | `B _ -> `R 
  | `L _ -> `L
  | `LA _ -> `LA 
  | `RGB (_, _) -> `RGB 
  | `RGBA (_, _) -> `RGBA 
  | `ARGB (_, _) -> `ARGB
	 
let pf_psize = function
  | (`A _ | `R _ | `G _ | `B _ | `L _ | 
     `RGB (_, (`P332 | `P565)) |
     `RGBA (_, (`P4444 | `P5551 | `P8888 | `P1010102)) |
     `ARGB (_, (`P4444 | `P1555 | `P8888 | `P2101010))) -> 1
  | `LA _ -> 2
  | `RGB (_, `C) -> 3
  | `RGBA (_, `C) -> 4
  | (`RGB (_, `DXT1) | (`RGBA (_, (`DXT1 | `DXT3 | `DXT5)))) -> -1

let pf_valid = function
  | (`RGB (s, (`P332 | `DXT1)) | `RGBA (s, (`DXT1 | `DXT3 | `DXT5))) -> 
      (match s with `Uint8 -> true | #storage -> false)
  | (`RGB (s, `P565) | `RGBA (s, (`P4444 | `P5551)) |
    `ARGB (s, (`P4444 | `P1555))) -> 
       (match s with `Uint16 -> true | #storage -> false)
  | (`RGBA (s, (`P8888 | `P1010102)) | `ARGB (s, (`P8888 | `P2101010))) -> 
      (match s with `Uint32 -> true | #storage -> false)
  | #pf -> true
 
let gl_enums_of_pf pf =  
  let k = function 
    | `Uint8 -> Gl.punsigned_byte
    | `Uint16 -> Gl.punsigned_short
    | `Uint32 -> Gl.punsigned_int
    | `Float32 -> Gl.pfloat
  in
  let enums f s = function
    | `C -> (f, k s)
    | `P332 -> (f, Gl.punsigned_byte_3_3_2)
    | `P565 -> (f, Gl.punsigned_short_5_6_5)
    | `P4444 -> 
	if f = Gl.bgra then (f, Gl.punsigned_short_4_4_4_4_rev) else
	(f, Gl.punsigned_short_4_4_4_4)
    | `P5551 -> (f, Gl.punsigned_short_5_5_5_1)
    | `P1555 -> (f, Gl.punsigned_short_1_5_5_5_rev)
    | `P8888 -> 
	if f = Gl.bgra then (f, Gl.punsigned_int_8_8_8_8_rev) else
	(f, Gl.punsigned_int_8_8_8_8)
    | `P1010102 -> (f, Gl.punsigned_int_10_10_10_2)
    | `P2101010 -> (f, Gl.punsigned_int_2_10_10_10_rev)
    | `DXT1 -> failwith "unimplemented" (* TODO *)
    | `DXT3 -> failwith "unimplemented"
    | `DXT5 -> failwith "unimplemented"
  in
  match pf with
  | `A s -> (Gl.alpha, k s)
  | `R s -> (Gl.red, k s)
  | `G s -> (Gl.green, k s)
  | `B s -> (Gl.blue, k s)
  | `L s -> (Gl.luminance, k s)
  | `LA s -> (Gl.luminance_alpha, k s)
  | `RGB (s, p) -> enums Gl.rgb s p 
  | `RGBA (s, p) -> enums Gl.rgba s p 
  | `ARGB (s, p) -> enums Gl.bgra s p

type 'a format = 
    { first : int;
      w_pitch : int;     
      h_pitch : int;
      w : int; 
      h : int;
      d : int;  
      pf : 'a pf;
      dim : dim;
      gl_pformat :  Gl.Enum.pformat;
      gl_pstorage : Gl.Enum.pstorage;
      psize : int; }
constraint 'a = [< storage ]

let format ?(first = 0) ?(w_skip = 0) ?(h_skip = 0) ~w ?(h = 1) ?(d = 1) 
    pf dim  =
  if not (pf_valid pf) then failwith err_invalid_pf;
  Check.igeq "first" first 0;
  if pf_compressed pf then begin 
    Check.ieq "width_skip" w_skip 0;
    Check.ieq "height_skip" h_skip 0;
    Check.eq _string_of_dim "dimension" dim `D2; 
  end else begin
    Check.igeq "width_skip" w_skip 0;
    Check.igeq "height_skip" h_skip 0;
  end;
  Check.igt "width" w 0;
  Check.igt "height" h 0;
  Check.igt "depth" d 0;    
  let gl_pformat, gl_pstorage = gl_enums_of_pf pf in 
  { first = first; 
    w_pitch = w + w_skip; 
    h_pitch = h + h_skip;
    w = w; 
    h = h; 
    d = d; 
    pf = pf;
    dim = dim;
    gl_pformat = gl_pformat;
    gl_pstorage = gl_pstorage;
    psize = pf_psize pf }
    
let first f = f.first 
let width_skip f = f.w_pitch - f.w
let height_skip f = f.h_pitch - f.h
let width f = f.w 
let height f = f.h 
let depth f = f.d 
let pf f = f.pf
let dim f = f.dim
let storage f = pf_storage f.pf
let comps f = pf_comps f.pf

let extent f = v3 (float_of_int f.w) (float_of_int f.h) (float_of_int f.d)
let extent2 f = v2 (float_of_int f.w) (float_of_int f.h)

let size f = 
  if pf_compressed f.pf then (* TODO f.w + 3, f.h + 3 *) 
    (f.w / 4) * (f.h / 4) * (block_size f.pf) (* bytes *)
  else
    f.w_pitch * f.h_pitch * f.d * f.psize 

let sub_format f ?(x = 0) ?(y = 0) ?(z = 0) ?(w = f.w - x) ?(h = f.h - y) 
    ?(d = f.d - z) ?(dim = f.dim) () =
  if pf_compressed f.pf then invalid_arg err_compressed;
  Check.irange "x" x 0 (f.w - 1);
  Check.irange "y" y 0 (f.h - 1);
  Check.irange "z" z 0 (f.d - 1);
  Check.irange "width" w 0 (f.w - x);
  Check.irange "height" h 0 (f.h - y);
  Check.irange "depth" d 0 (f.d - z);
  Check.leq _string_of_dim "dimension" dim f.dim;
  { f with
    first = f.first + z * f.h_pitch * f.w_pitch + y * f.w_pitch + x;
    w_pitch = f.w_pitch + (f.w - w);
    h_pitch = f.h_pitch + (f.h - h);
    w = w;
    h = h;
    d = d;
    dim = dim; }

let print_format fmt f =
  let pf_string : [< storage] pf -> string  = function pf ->  
    let k = function 
      | `Uint8 -> "`Uint8" 
      | `Uint16 -> "`Uint16"  
      | `Uint32 -> "`Uint32"  
      | `Float32 -> "`Float32" 
    in
    let l = function
      | `C -> ", `C)"
      | `P332 -> ", `P332)"
      | `P565 -> ", `P565)"
      | `P4444 -> ", `P4444)"
      | `P5551 -> ", `P5551)"
      | `P1555 -> ", `P1555)"
      | `P8888 -> ", `P8888)"
      | `P1010102 -> ", `P1010102)"
      | `P2101010 -> ", `P2101010)"
      | `DXT1 -> ", `DXT1)"
      | `DXT3 -> ", `DXT3)"
      | `DXT5 -> ", `DXT5)"
    in
    match pf with
    | `A s -> "`A " ^ (k s)
    | `R s -> "`R " ^ (k s)
    | `G s -> "`G " ^ (k s)
    | `B s -> "`B " ^ (k s)
    | `L s -> "`L " ^ (k s)
    | `LA s -> "`LA " ^ (k s)
    | `RGB (s, p) -> "`RGB (" ^ (k s) ^ (l p) 
    | `RGBA (s, p) -> "`RGBA (" ^ (k s) ^ (l p)
    | `ARGB (s, p) -> "`ARGB (" ^ (k s) ^ (l p)
  in
  Format.fprintf fmt
    "@[<hov 1>{first =@ %d;@ width_skip =@ %d;@ height_skip =@ %d;\
     @ width =@ %d;@ height =@ %d;@ depth =@ %d;@ pf =@ %s;@ dim =@ %s}@]"
  f.first (width_skip f) (height_skip f) f.w f.h f.d 
  (pf_string  f.pf) (_string_of_dim f.dim)

type 'a t = 'a Buf.t * 'a format

let pos (b, f) ~x ~y = f.first + y * f.w_pitch + x 
let pos3 (b, f) ~x ~y ~z =
  f.first + z * f.h_pitch * f.w_pitch + y * f.w_pitch + x

let pixel_size (b, f) = f.psize
let sub (b, f) ?x ?y ?z ?w ?h ?d ?dim () = 
  (b, sub_format f ?x ?y ?z ?w ?h ?d ?dim ())

let cast s ((b, f) as i) = 
  Check.spec _string_of_storage (pf_storage f.pf) s; i


let print fmt ?(x = 0) ?(y = 0) ?(z = 0) ?(w = max_int)
    ?(h = max_int) ?(d = max_int) (b, f) =
  (* TODO doesn't work with compressed formats. *)
  let w' = if w = max_int then f.w - x else w in
  let h' = if h = max_int then f.h - y else h in
  let d' = if d = max_int then f.d - z else d in
  Check.irange "x" x 0 (f.w - 1);
  Check.irange "y" y 0 (f.h - 1);
  Check.irange "z" z 0 (f.d - 1);
  Check.irange "width" w' 0 (f.w - x);
  Check.irange "height" h' 0 (f.h - y);
  Check.irange "depth" d' 0 (f.d - z);
  let print k el_fmt b = 
    let pr s = Format.fprintf fmt s in
    let iter a =
      let pr_pixel a pos = pr el_fmt a.{pos} in
      let pr_pixel_comps a pos = 
	pr "(";
	pr el_fmt a.{pos};
	for i = pos + 1 to pos + f.psize - 1 do
	  pr " ";
	  pr el_fmt a.{i};
	done;
	pr ")"
      in
      let pr_line a pos = 
	let pos x = pos + x * f.psize in 
	let pr_pixel = if f.psize = 1 then pr_pixel else pr_pixel_comps in
	pr "@[<h 1>[";
	if w' > 0 then begin
	  pr_pixel a (pos x);
	  for i = x + 1 to x + w' - 1 do 
	    pr ";@ ";
	    pr_pixel a (pos i);
	  done
	end;
	pr "]@]"
      in
      let pr_image a pos = 
	let pos y = pos + y * f.w_pitch in
	pr "@[<v 1>[";
	if h' > 0 then begin
	  pr_line a (pos (y + h' - 1));
	  for i = y + h' - 2 downto  y do 
	    pr ";@ ";
	    pr_line a (pos i)
	  done
	end;
	pr "]@]"
      in
      let pos z = f.first + z * f.h_pitch * f.w_pitch in
      pr_image a (pos z);
      for i = z + 1 to z + d' - 1 do 
	pr ";@ ";
	pr_image a (pos i)
      done
    in
    pr "@[[<1>"; if d' > 0 then Buf.map k `Read iter b; pr "]@]"
  in
  match Buf.kind b with
  | `Int8 -> print Buf.int8 "%X" (Buf.cast `Int8 b)
  | `Uint8 -> print Buf.uint8 "%X" (Buf.cast `Uint8 b)
  | `Int16 -> print Buf.int16 "%X" (Buf.cast `Int16 b)
  | `Uint16 -> print Buf.uint16 "%X" (Buf.cast `Uint16 b)
  | `Int32 -> print Buf.int32 "%lX" (Buf.cast `Int32 b)
  | `Uint32 -> print Buf.uint32 "%lX" (Buf.cast `Uint32 b)
  | `Float32 -> print Buf.float32 "%F" (Buf.cast `Float32 b)
  | `Float64 -> print Buf.float64 "%F" (Buf.cast `Float64 b)


let _gl_pformat f = f.gl_pformat
let _gl_pstorage f = f.gl_pstorage
let _gl_set_pixel_pack ft f = 
  Gl.pixel_storei ft Gl.pack_row_length f.w_pitch;
  Gl.pixel_storei ft Gl.pack_image_height f.h_pitch

let _gl_set_pixel_unpack ft f = 
  Gl.pixel_storei ft Gl.unpack_row_length f.w_pitch;
  Gl.pixel_storei ft Gl.unpack_image_height f.h_pitch
