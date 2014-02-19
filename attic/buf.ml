(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

exception Corruption

type aim = [ `Attr | `Index | `Pixel_read | `Pixel_write ]
let gl_enum_of_aim = function
  | `Attr -> Gl.array_buffer
  | `Index -> Gl.element_array_buffer
  | `Pixel_write -> Gl.pixel_pack_buffer
  | `Pixel_read -> Gl.pixel_unpack_buffer

type usage = [ (* TODO maybe change to something more understandable. *)
  | `Stream_draw | `Stream_read | `Stream_copy
  | `Static_read | `Static_draw | `Static_copy
  | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]
      
let gl_enum_of_usage = function
  | `Stream_draw -> Gl.stream_draw
  | `Stream_read -> Gl.stream_read
  | `Stream_copy -> Gl.stream_copy
  | `Static_draw -> Gl.static_draw
  | `Static_read -> Gl.static_read  
  | `Static_copy -> Gl.static_copy
  | `Dynamic_draw -> Gl.dynamic_draw
  | `Dynamic_read -> Gl.dynamic_read
  | `Dynamic_copy -> Gl.dynamic_copy

let usage_of_gl_enum = function 
  | e when (e = Gl.stream_draw) -> `Stream_draw
  | e when (e = Gl.stream_read) -> `Stream_read
  | e when (e = Gl.stream_copy) -> `Stream_copy
  | e when (e = Gl.static_draw) -> `Static_draw
  | e when (e = Gl.static_read) -> `Static_read
  | e when (e = Gl.static_copy) -> `Static_copy
  | e when (e = Gl.dynamic_draw) -> `Dynamic_draw
  | e when (e = Gl.dynamic_read) -> `Dynamic_read
  | e when (e = Gl.dynamic_copy) -> `Dynamic_copy
  | e -> assert false
	
type kind = [
  | `Int8 | `Uint8 | `Int16 | `Uint16 | `Int32 | `Uint32 | `Float32 | `Float64 ]
type float_kind = [ `Float32 | `Float64 ] 
type int_kind  = [ `Int16 | `Int32 | `Int8 ] 
type uint_kind  = [ `Uint16 | `Uint32 | `Uint8 ] 
let _string_of_kind = function 
  | `Int8 -> "`Int8" 
  | `Uint8 -> "`Uint8" 
  | `Int16 -> "`Int16" 
  | `Uint16 -> "`Uint16" 
  | `Int32 -> "`Int32" 
  | `Uint32 -> "`Uint32" 
  | `Float32 -> "`Float32" 
  | `Float64 -> "`Float64"

let _kind_bytes = function
  | (`Int8  | `Uint8) -> 1
  | (`Int16 | `Uint16) -> 2 
  | (`Int32 | `Uint32 | `Float32) -> 4
  |  `Float64 -> 8

type +'a t = 
    { gl_usage : Gl.Enum.buf_usage;
      kind : 'a;
      mutable length : int;                 (* mutable only for Buf.destroy. *)
      kind_bytes : int;
      mutable g : Gl.Ptr.g;                 (* mutable only for Buf.destroy. *)
      ft : Gl.Ptr.ft; }
constraint 'a = [< kind ]

let finalise b =
  Gc.finalise_release ();
  if b.g <> Gl.Ptr.g_nil then Gl.delete_buffer b.ft b.g

let create r ?(aim = `Attr) ?(usage = `Static_draw) kind len =
  Check.igeq "length" len 0;
  let kind_bytes = _kind_bytes kind in
  let gl_usage = gl_enum_of_usage usage in
  let target = gl_enum_of_aim aim in
  let ft = Rend._ft r in
  let g = Gl.gen_buffer ft in
  Gl.bind_buffer ft target g;
  Gl.buffer_data ft target kind_bytes len Gl.Ptr.c_nil gl_usage;  
  let b = 
    { gl_usage = gl_usage; 
      kind = kind;
      length = len;
      kind_bytes = kind_bytes;
      g = g; 
      ft = ft }    
  in
  Gc.finalise finalise b;
  b

let destroy b =                                     (* Do we want this fun ? *)
  Gl.delete_buffer b.ft b.g;
  b.g <- Gl.Ptr.g_nil;
  b.length <- 0

let usage b = (usage_of_gl_enum b.gl_usage)
let kind b = b.kind
let length b = b.length
let cast kind b = Check.spec _string_of_kind b.kind kind; b      
let print f b = 
  let kind = _string_of_kind b.kind in
  Format.fprintf f "@[<1>{kind =@ %s;@ length =@ %d}@]" kind b.length 

type ('a, 'b) barray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
type ('a, 'b, 'c) array_kind = ('b, 'c) Bigarray.kind
let int8 = Bigarray.int8_signed
let uint8 = Bigarray.int8_unsigned
let int16 = Bigarray.int16_signed
let uint16 = Bigarray.int16_unsigned
let int32 = Bigarray.int32
let uint32 = Bigarray.int32
let float32 = Bigarray.float32
let float64 = Bigarray.float64

type access = [ `Read | `Write | `Read_write | `Discard ]
let enum_of_access = function
  | `Read -> Gl.read_only
  | `Write -> Gl.write_only
  | `Read_write -> Gl.read_write
  | `Discard -> Gl.write_only 

let t = Gl.array_buffer           (* Target used for copy and map operations *)

let map k access f b =
  Gl.bind_buffer b.ft t b.g;
  if access = `Discard then 
    Gl.buffer_data b.ft t b.kind_bytes b.length Gl.Ptr.c_nil b.gl_usage;
  let c = Gl.map_buffer b.ft t (enum_of_access access) in
  if c = Gl.Ptr.c_nil then raise Out_of_memory;
  let a = Gl.Ptr.barray_of_c c k b.length in
  let unmap () = 
    Gl.Ptr.barray_invalidate a;
    Gl.bind_buffer b.ft t b.g;                 (* in case of multiple maps ! *)
    if not (Gl.unmap_buffer b.ft t) then raise Corruption
  in
  (try f a with e -> (unmap (); raise e));
  unmap ()

let copy_c_to_g ft src dst dst_pos len =                         
  let discard = (len = dst.length) in
  Gl.bind_buffer ft t dst.g;
  if discard then Gl.buffer_data ft t dst.kind_bytes len src dst.gl_usage else 
  Gl.buffer_sub_data ft t dst.kind_bytes dst_pos len src

let copy_g_to_c ft src src_pos dst len =                         
  Gl.bind_buffer ft t src.g;
  Gl.get_buffer_sub_data ft t src.kind_bytes src_pos len dst
	      
let blit ~src ~src_pos ~dst ~dst_pos ~len = 
  Check.irange "src_pos" src_pos 0 src.length;
  Check.irange "len" len 0 (src.length - src_pos);
  Check.irange "dst_pos" dst_pos 0 dst.length;
  Check.irange "len" len 0 (dst.length - dst_pos);
  let map_src a =
    let c = Gl.map_buffer src.ft t a in
    if c = Gl.Ptr.c_nil then raise Out_of_memory else c
  in
  let unmap_src () = 
    Gl.bind_buffer src.ft t src.g; 
    if not (Gl.unmap_buffer src.ft t) then raise Corruption
  in
  if src == dst then
    begin
      Gl.bind_buffer src.ft t src.g;
      let c = map_src Gl.read_write in
      let s = Gl.Ptr.c_offset c src.kind_bytes src_pos in
      let d = Gl.Ptr.c_offset c dst.kind_bytes dst_pos in
      Gl.Ptr.c_copy s d src.kind_bytes len;
      if not (Gl.unmap_buffer src.ft t) then raise Corruption
    end 
  else 
    begin
      Gl.bind_buffer src.ft t src.g;
      let c = map_src Gl.read_only in
      let s = Gl.Ptr.c_offset c src.kind_bytes src_pos in
      Gl.bind_buffer dst.ft t dst.g;
      let c' = Gl.map_buffer dst.ft t Gl.write_only in
      if c' = Gl.Ptr.c_nil then (unmap_src (); raise Out_of_memory);
      let d = Gl.Ptr.c_offset c' dst.kind_bytes dst_pos in
      Gl.Ptr.c_copy s d src.kind_bytes len;
      if not (Gl.unmap_buffer dst.ft t) then (unmap_src (); raise Corruption) 
      else
      unmap_src ()
    end

let blit_from_bigarray k ~src ~src_pos ~dst ~dst_pos ~len =
  Check.irange "src_pos" src_pos 0 (Bigarray.Array1.dim src);
  Check.irange "len" len 0 ((Bigarray.Array1.dim src) - src_pos);
  Check.irange "dst_pos" dst_pos 0 dst.length;
  Check.irange "len" len 0 (dst.length - dst_pos);
  let src' = Gl.Ptr.c_offset (Gl.Ptr.c_of_barray src) dst.kind_bytes src_pos in
  copy_c_to_g dst.ft src' dst dst_pos len
 
let blit_to_bigarray k ~src ~src_pos ~dst ~dst_pos ~len = 
  Check.irange "src_pos" src_pos 0 src.length;
  Check.irange "len" len 0 (src.length - src_pos);
  Check.irange "dst_pos" dst_pos 0 (Bigarray.Array1.dim dst);
  Check.irange "len" len 0 (Bigarray.Array1.dim dst);
  let dst' = Gl.Ptr.c_offset (Gl.Ptr.c_of_barray dst) src.kind_bytes dst_pos in
  copy_g_to_c src.ft src src_pos dst' len

let print_data fmt ?(pos = 0) ?(len = max_int) b = 
  let len' = if len = max_int then (b.length - pos) else len in
  Check.irange "pos" pos 0 b.length;
  Check.irange "len" len' 0 (b.length - pos);
  let print k el_fmt =  
    let pr s = Format.fprintf fmt s in
    let iter a = 
      pr el_fmt a.{pos};
      for i = pos + 1 to pos + len' - 1 do pr ";@ "; pr el_fmt a.{i} done
    in
    pr "@[<1>["; if len' > 0 then map k `Read iter b; pr "]@]"
  in
  match b.kind with
  | `Int8 -> print int8 "%d"
  | `Uint8 -> print uint8 "%d"
  | `Int16 -> print int16 "%d" 
  | `Uint16 -> print uint16 "%d"
  | `Int32 -> print int32 "%ld"
  | `Uint32 -> print uint32 "%lu"
  | `Float32 -> print float32 "%F"
  | `Float64 -> print float64 "%F"

let _gl_storage b = match b.kind with
| `Int8    -> Gl.abyte
| `Uint8   -> Gl.aunsigned_byte
| `Int16   -> Gl.ashort
| `Uint16  -> Gl.aunsigned_short
| `Int32   -> Gl.aint
| `Uint32  -> Gl.aunsigned_int
| `Float32 -> Gl.afloat
| `Float64 -> Gl.adouble
      
let _gl_bind_attribute b offset = 
  Gl.bind_buffer b.ft Gl.array_buffer b.g;
  Gl.Ptr.c_offset Gl.Ptr.c_nil b.kind_bytes offset 
    
let _gl_bind_index b offset = 
  Gl.bind_buffer b.ft Gl.element_array_buffer b.g;
  Gl.Ptr.c_offset Gl.Ptr.c_nil b.kind_bytes offset 
    
let _gl_bind_pixel_pack b offset =
  Gl.bind_buffer b.ft Gl.pixel_pack_buffer b.g;
  Gl.Ptr.c_offset Gl.Ptr.c_nil b.kind_bytes offset 

let _gl_bind_pixel_unpack b offset =
  Gl.bind_buffer b.ft Gl.pixel_unpack_buffer b.g;
  Gl.Ptr.c_offset Gl.Ptr.c_nil b.kind_bytes offset

let _ft b = b.ft
