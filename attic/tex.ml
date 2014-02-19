(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

open Gg;;

let err_rect_mip = "rectangle textures cannot use mipmap filtering"
let err_mip_level l = Printf.sprintf "invalid mipmap level (%d)" l

let pow_two v = (v land (v - 1)) = 0

let ceil_pow2 i = 
  let i = i - 1 in
  let i = i lor (i lsr 1) in
  let i = i lor (i lsr 2) in
  let i = i lor (i lsr 4) in
  let i = i lor (i lsr 8) in
  let i = i lor (i lsr 16) in
  i + 1

let ceil_pow_two v = 
  let rec loop i v = if v = 0 then i else loop (i + 1) (v lsr 1) in
  let pot = 1 lsl (loop 0 v) in
  if (pot lsr 1) = v then v else pot

type pf = [
  | `A of [ `C8 | `C16 | `C16f | `C32f ] 
  | `D of [ `C16 | `C24 | `C32 ]
  | `I of [ `C8 | `C16 | `C16f | `C32f ]
  | `L of [ `C8 | `C16 | `C16f | `C32f ]
  | `LA of [ `C4 | `C8 | `C16 | `C16f | `C32f ]
  | `RGB of [ `C8 | `P332 | `P555 | `C16 | `C16f | `C32f | `DXT1 ]
  | `RGBA of [ `C2 | `C4 | `P5551 | `C8 | `P1010102 | `C16 | `C16f | `C32f | 
               `DXT1 | `DXT3 | `DXT5 ]]

let gl_enum_of_pf = function
  | `A `C8 -> Gl.alpha8
  | `A `C16 -> Gl.alpha16
  | `A `C16f -> Gl.alpha16f
  | `A `C32f -> Gl.alpha32f
  | `D `C16 -> Gl.depth_component16
  | `D `C24 -> Gl.depth_component24
  | `D `C32 -> Gl.depth_component32
  | `I `C8 -> Gl.intensity8
  | `I `C16 -> Gl.intensity16
  | `I `C16f -> Gl.intensity16f
  | `I `C32f -> Gl.intensity32f
  | `L `C8 -> Gl.luminance8
  | `L `C16 -> Gl.luminance16
  | `L `C16f -> Gl.luminance16f
  | `L `C32f -> Gl.luminance32f
  | `LA `C4 -> Gl.luminance4_alpha4
  | `LA `C8 -> Gl.luminance8_alpha8
  | `LA `C16 -> Gl.luminance16_alpha16
  | `LA `C16f -> Gl.luminance_alpha16f
  | `LA `C32f -> Gl.luminance_alpha32f
  | `RGB `P332 -> Gl.r3_g3_b2
  | `RGB `P555 -> Gl.rgb5
  | `RGB `C8 -> Gl.rgb8
  | `RGB `C16 -> Gl.rgb16
  | `RGB `C16f -> Gl.rgb16f
  | `RGB `C32f -> Gl.rgb32f
  | `RGB `DXT1 -> Gl.compressed_rgb_s3tc_dxt1
  | `RGBA `C2 -> Gl.rgba2
  | `RGBA `C4 -> Gl.rgba4
  | `RGBA `P5551 -> Gl.rgb5_a1
  | `RGBA `C8 -> Gl.rgba8
  | `RGBA `P1010102 -> Gl.rgb10_a2
  | `RGBA `C16 -> Gl.rgba16
  | `RGBA `C16f -> Gl.rgba16f
  | `RGBA `C32f -> Gl.rgba32f
  | `RGBA `DXT1 -> Gl.compressed_rgba_s3tc_dxt1
  | `RGBA `DXT3 -> Gl.compressed_rgba_s3tc_dxt3
  | `RGBA `DXT5 -> Gl.compressed_rgba_s3tc_dxt5

let compressed_format = function
  | (`RGB `DXT1 | `RGBA (`DXT1 | `DXT3 | `DXT5)) -> true
  | #pf -> false

let float_format = function
  | (`A (`C16f | `C32f) | `I (`C16f | `C32f) | `L (`C16f | `C32f) | 
     `LA (`C16f | `C32f) | `RGB (`C16f | `C32f) | `RGBA (`C16f | `C32f)) -> true
  | #pf -> false

let nearest_pf pf =
  let sc = function 
    | `Uint8 -> `C8 | `Uint16 -> `C16 | `Uint32 -> `C16 | `Float32 -> `C32f
  in
  match pf with
  | `R s -> `RGB (sc s)
  | `G s -> `RGB (sc s)
  | `B s -> `RGB (sc s)
  | `A s -> `A (sc s)
  | `L s -> `L (sc s)
  | `LA s -> `LA (sc s)
  | `RGB (s, p) -> 
      begin match p with
      | `C -> `RGB (sc s)
      | `DXT1 -> `RGB `DXT1
      | `P332 -> `RGB `P332
      | `P565 -> `RGB `P555
      end
  | `RGBA (s, p) ->
      begin match p with 
      | `C -> `RGBA (sc s)
      | `P4444 -> `RGBA `C4
      | `P5551 -> `RGBA `P5551
      | `P8888 -> `RGBA `C8
      | `P1010102 -> `RGBA `P1010102
      | `DXT1 -> `RGBA `DXT1
      | `DXT3 -> `RGBA `DXT3
      | `DXT5 -> `RGBA `DXT5
      end
  | `ARGB (s, p) ->
      begin match p with 
      | `C -> `RGBA (sc s)
      | `P4444 -> `RGBA `C4
      | `P1555 -> `RGBA `P5551
      | `P8888 -> `RGBA `C8
      | `P2101010 -> `RGBA `P1010102
      | `DXT1 -> `RGBA `DXT1
      | `DXT3 -> `RGBA `DXT3
      | `DXT5 -> `RGBA `DXT5
      end

type cube_face = [ `Pos_x | `Neg_x | `Pos_y | `Neg_y | `Pos_z | `Neg_z ]

let gl_enum_of_cube_face = function
  | `Pos_x -> Gl.ttexture_cube_map_positive_x
  | `Neg_x -> Gl.ttexture_cube_map_negative_x
  | `Pos_y -> Gl.ttexture_cube_map_positive_y
  | `Neg_y -> Gl.ttexture_cube_map_negative_y
  | `Pos_z -> Gl.ttexture_cube_map_positive_z
  | `Neg_z -> Gl.ttexture_cube_map_negative_z

type mag_f = [ `Nearest | `Linear]      
type min_f = [ 
  | mag_f | `Nearest_mipmap_nearest | `Nearest_mipmap_linear
  | `Linear_mipmap_nearest | `Linear_mipmap_linear ]

let gl_enum_of_filter = function
  | `Nearest -> Gl.nearest 
  | `Linear -> Gl.linear
  | `Nearest_mipmap_nearest -> Gl.nearest_mipmap_nearest
  | `Nearest_mipmap_linear -> Gl.nearest_mipmap_linear
  | `Linear_mipmap_nearest -> Gl.linear_mipmap_nearest
  | `Linear_mipmap_linear -> Gl.linear_mipmap_linear

type wrap = [ 
  | `Clamp | `Clamp_to_border | `Clamp_to_edge | `Repeat |  `Mirrored_repeat ]
(* TODO `Mirrored_repeat needs ARB_texture_mirrored_repeat OpenGL 1.4 *)
         
let gl_enum_of_wrap = function
  | `Clamp -> Gl.clamp
  | `Clamp_to_edge -> Gl.clamp_to_edge
  | `Repeat -> Gl.repeat
  | `Clamp_to_border -> Gl.clamp_to_border
  | `Mirrored_repeat -> Gl.mirrored_repeat

type kind = [ `D1 | `D2 | `D2_rect | `D3 | `Cube_map ]
let gl_enum_of_kind = function
  | `D1 -> Gl.ttexture_1D
  | `D2 -> Gl.ttexture_2D
  | `D2_rect -> Gl.ttexture_rectangle
  | `D3 -> Gl.ttexture_3D
  | `Cube_map -> Gl.ttexture_cube_map

type 'a t = 
    { mutable gl_priority : float;
      mutable gl_min_f : min_f;
      mutable gl_mag_f : mag_f;
      mutable gl_lod_bias : float;
      mutable gl_max_level : int;
      mutable gl_wrap_s : wrap;
      mutable gl_wrap_t : wrap;
      mutable gl_wrap_r : wrap;
      w : int;
      h : int;
      d : int;
      pf : pf;
      kind : 'a; 
      mutable g : Gl.Ptr.g;
      gl_target : Gl.Enum.tex_target;
      gl_pformat : Gl.Enum.tex_pformat;
      ft : Gl.Ptr.ft } 
constraint 'a = [< kind]
	
let finalise t = 
  Gc.finalise_release ();
  Gl.delete_texture t.ft t.g

let create r ?(priority = 1.0) ?(min_f = `Nearest_mipmap_linear) 
    ?(mag_f = `Linear) ?(lod_bias = 0.0)  ?(max_level = 0) 
    ?(wrap_s = `Repeat) ?(wrap_t = `Repeat) ?(wrap_r = `Repeat) 
    ~w ?(h = 1) ?(d = 1) pf kind = 
  let max_max_level w h d c =
    let m = float_of_int (max (max w h) d) in
    min (truncate (log m /. log 2.)) c 
  in
  Check.igt "width" w 0;
  Check.igt "height" h 0;
  Check.igt "depth" d 0;
  Check.igeq "max_level" max_level 0; 
  begin match kind with 
  | `D2_rect -> 
      Rend.srect_textures r;
      if max_level > 0 then invalid_arg err_rect_mip;
  | #kind ->
      if not ((pow_two w) && (pow_two h) && (pow_two d)) then
	Rend.snpot_textures r
  end;
  if compressed_format pf then Rend.sdxt_textures r;
  if float_format pf then Rend.sfloat_textures r;
  let   ft = Rend._ft r in
  let t = 
    { gl_priority = priority;
      gl_min_f = min_f;
      gl_mag_f = mag_f;
      gl_lod_bias = lod_bias;
      gl_max_level = (max_max_level w h d max_level);
      gl_wrap_s = wrap_s;
      gl_wrap_t = wrap_t;
      gl_wrap_r = wrap_r;
      w = w;
      h = h;
      d = d;
      pf = pf;
      kind = kind; 
      g = Gl.gen_texture ft; 
      gl_target = gl_enum_of_kind kind; 
      gl_pformat = gl_enum_of_pf pf;
      ft = ft; } 
  in
  Gc.finalise finalise t;
  Gl.bind_texture ft t.gl_target t.g;
  let f = Gl.tex_parameterf ft t.gl_target in
  let i = Gl.tex_parameteri ft t.gl_target in
  f Gl.texture_priority t.gl_priority;
  i Gl.texture_min_filter (gl_enum_of_filter t.gl_min_f);
  i Gl.texture_mag_filter (gl_enum_of_filter t.gl_mag_f);
  f Gl.texture_lod_bias t.gl_lod_bias;
  i Gl.texture_base_level 0;
  i Gl.texture_max_level t.gl_max_level;
  i Gl.texture_wrap_s (gl_enum_of_wrap t.gl_wrap_s);
  i Gl.texture_wrap_t (gl_enum_of_wrap t.gl_wrap_t);
  i Gl.texture_wrap_r (gl_enum_of_wrap t.gl_wrap_r);    
  (* Avoid reading from a bound buffer. *)
  Gl.bind_buffer (Rend._ft r) Gl.pixel_unpack_buffer Gl.Ptr.g_nil;
  begin match kind with
 (* TODO check passing GL.rgba with any gl_pformat doesn't generate errors. *)
  | `D1 -> 
      for i = 0 to max_level do
	Gl.tex_image_1d ft t.gl_target ~level:i t.gl_pformat ~w:t.w
	  ~border:0 Gl.rgba Gl.pbyte Gl.Ptr.c_nil 
      done
  | (`D2 | `D2_rect) ->
      for i = 0 to max_level do
	Gl.tex_image_2d ft t.gl_target ~level:i t.gl_pformat ~w:t.w ~h:t.h 
	  ~border:0 Gl.rgba Gl.pbyte Gl.Ptr.c_nil 
      done
  | `D3 ->
      for i = 0 to max_level do
	Gl.tex_image_3d ft t.gl_target ~level:i t.gl_pformat ~w:t.w ~h:t.h 
	  ~d:t.d ~border:0 Gl.rgba Gl.pbyte Gl.Ptr.c_nil 
      done
  | `Cube_map -> 
      let set tgt = 
	Gl.tex_image_2d ft tgt ~level:0 t.gl_pformat ~w:t.w ~h:t.h
	  ~border:0 Gl.rgba Gl.pbyte Gl.Ptr.c_nil
      in
      for i = 0 to max_level do
	set Gl.ttexture_cube_map_positive_x;
	set Gl.ttexture_cube_map_negative_x;
	set Gl.ttexture_cube_map_positive_y;
	set Gl.ttexture_cube_map_negative_y;
	set Gl.ttexture_cube_map_positive_z;
	set Gl.ttexture_cube_map_negative_z;
      done
  end;
  t

let destroy t = Gl.delete_texture t.ft t.g; t.g <- Gl.Ptr.g_nil

let priority t = t.gl_priority
let set_priority t p =
  t.gl_priority <- p;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameterf t.ft t.gl_target Gl.texture_priority p

let min_f t = t.gl_min_f
let set_min_f t f = 
  t.gl_min_f <- f;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.texture_min_filter (gl_enum_of_filter f)

let mag_f t = t.gl_mag_f
let set_mag_f t f =
  t.gl_mag_f <- f;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.texture_mag_filter (gl_enum_of_filter f)

let lod_bias t = t.gl_lod_bias 
let set_lod_bias t b = 
  t.gl_lod_bias <- b;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameterf t.ft t.gl_target Gl.texture_lod_bias b

let max_level t = t.gl_max_level

let wrap_s t = t.gl_wrap_s
let set_wrap_s t w = 
  t.gl_wrap_s <- w;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.texture_wrap_s (gl_enum_of_wrap w)

let wrap_t t = t.gl_wrap_t
let set_wrap_t t w = 
  t.gl_wrap_t <- w;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.texture_wrap_t (gl_enum_of_wrap w)

let wrap_r t = t.gl_wrap_r
let set_wrap_r t w = 
  t.gl_wrap_r <- w;
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.texture_wrap_r (gl_enum_of_wrap w)

let width t = t.w
let height t = t.h
let depth t = t.d
let pf t = t.pf
let kind t = t.kind
let extent t = v3 (float_of_int t.w) (float_of_int t.h) (float_of_int t.d)
let extent2 t = v2 (float_of_int t.w) (float_of_int t.h)
let resident t = 
  Gl.bind_texture t.ft t.gl_target t.g;
  if (Gl.get_tex_parameteri t.ft t.gl_target Gl.texture_resident) = 1 then true 
  else false

let print fmt t = 
  let kind = match t.kind with
  | `D1 -> "`D1"
  | `D2 -> "`D2"
  | `D2_rect -> "`D2_rect"
  | `D3 -> "`D3"
  | `Cube_map -> "`Cube_map"
  in
  let layout = function
    | `C2 -> " `C2"
    | `P332 -> " `P332"
    | `C4 -> " `C4"
    | `P555 -> " `P555"
    | `P5551 -> " `P5551"
    | `C8 -> " `C8"
    | `P1010102 -> " `P101010102"
    | `C16 -> " `C16"
    | `C16f -> " `C16f"
    | `C24 -> " `C24"
    | `C32 -> " `C32"
    | `C32f -> " `C32f"
    | `DXT1 -> " `DXT1"
    | `DXT3 -> " `DXT3"
    | `DXT5 -> " `DXT5"
  in
  let format = match t.pf with
  | `A l -> "`A" ^ (layout l)
  | `D l -> "`D" ^ (layout l)
  | `I l -> "`I" ^ (layout l)
  | `L l -> "`L" ^ (layout l)
  | `LA l -> "`LA" ^ (layout l)
  | `RGB l -> "`RGB" ^ (layout l)
  | `RGBA l -> "`RGBA" ^ (layout l)
  in
  Format.fprintf fmt 
  "@[<1>{width = %d;@,@ height = %d;@,@ depth = %d;@,@ format = %s;@,@ \
      kind = %s}@]" 
  t.w t.h t.d format kind

let mipmap_bounds t ~level = 
  Check.irange "level" level 0 t.gl_max_level;
  (max 1 (t.w / (1 lsl level))), 
  (max 1 (t.h / (1 lsl level))),
  (max 1 (t.d / (1 lsl level))) 

let compute_mipmaps t = (* TODO use fbo function, this shouldn't work *)
  Gl.bind_texture t.ft t.gl_target t.g;
  Gl.tex_parameteri t.ft t.gl_target Gl.generate_mipmap 1

let blit_from_image (b,f) ?(level = 0) ?(face = `Pos_x) ?(x = 0) ?(y = 0) 
    ?(z = 0)  t = 
  Check.ileq "mipmap level" level t.gl_max_level;
  let w = Image.width f in
  let h = Image.height f in
  let d = Image.depth f in
  let pf = Image._gl_pformat f in
  let ps = Image._gl_pstorage f in
  match t.kind with
  | `D1 ->
      Check.irange "x" x 0 (t.w - 1);
      Check.eq Image._string_of_dim "image dimension" (Image.dim f) `D1;
      if Image.pf_compressed (Image.pf f) then 
	failwith "unimplemented";
      Image._gl_set_pixel_unpack t.ft f;
      Gl.bind_texture t.ft t.gl_target t.g;
      let c = Buf._gl_bind_pixel_unpack b (Image.first f) in
      Gl.tex_sub_image_1d t.ft t.gl_target level x w pf ps c 
  | (`D2 | `D2_rect | `Cube_map) -> 
      Check.irange "x" x 0 (t.w - 1);
      Check.irange "y" y 0 (t.h - 1);
      Check.eq Image._string_of_dim "image dimension" (Image.dim f) `D2;
      if Image.pf_compressed (Image.pf f) then 
	failwith "unimplemented";
      Image._gl_set_pixel_unpack t.ft f;
      Gl.bind_texture t.ft t.gl_target t.g;
      let tg = match t.kind with 
      | `Cube_map -> gl_enum_of_cube_face face 
      | #kind -> t.gl_target
      in
      let c = Buf._gl_bind_pixel_unpack b (Image.first f) in
      Gl.tex_sub_image_2d t.ft tg level x y w h pf ps c
  | `D3 -> 
      Check.irange "x" x 0 (t.w - 1);
      Check.irange "y" y 0 (t.h - 1);
      Check.irange "z" z 0 (t.d - 1);
      Check.eq Image._string_of_dim "image dimension" (Image.dim f) `D3;
      if Image.pf_compressed (Image.pf f) then 
	failwith "unimplemented";
      Image._gl_set_pixel_unpack t.ft f;
      Gl.bind_texture t.ft t.gl_target t.g;
      let c = Buf._gl_bind_pixel_unpack b (Image.first f) in
      Gl.tex_sub_image_3d t.ft t.gl_target level x y z w h d pf ps c

let blit_to_image ?level ?face t (b,f) = 
  failwith "unimplemented" (* GlGetTexImage *)

let _gl_bind_texture t = 
  Gl.bind_texture t.ft t.gl_target t.g;
