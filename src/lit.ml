(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg

let str = Printf.sprintf 
let pp = Format.fprintf
let pp_str = Format.pp_print_string
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function 
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

let to_string_of_formatter pp v =                        (* NOT thread safe. *)
  Format.fprintf Format.str_formatter "%a" pp v; 
  Format.flush_str_formatter ()

(* Invalid argument strings *) 

let err_miss_key n k = str "no %s %s" n k
let err_miss_attr k = str "no attribute %s" k 
let err_attr_dim dim = str "invalid attribute dimension %d" dim 
let err_no_cpu_buffer = str "no cpu buffer" 
let err_ba_kind kind = str "unsupported bigarray kind for Lit.Buf" 
let err_ba_kind_mismatch exp = 
  str "bigarray kind mismatch (scalar type is %s)" exp (* TODO found: ba *)

let err_prim_underspec = str "one of ?index or ?count must be specified"
let err_prim_not_uint t = str "index's scalar type not unsigned integer (%s)" t
let err_prim_attr_dup n = str "attribute %s specified more than once" n

let err_miss_uniform k = str "no uniform %s" k

(* Renderer ids. *) 

module Id = struct
  type t = int 
  let compare : int -> int -> int = Pervasives.compare
end

(* Renderer info *) 

module Info = struct
  type t = exn         (* universal type, see http://mlton.org/UniversalType *) 
  let create (type s) () = 
    let module M = struct exception E of s option end in 
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  let none = fst (create ()) None
end

(* Buffers *) 

module Buf = struct

  (* FIXME: this module has three uses of Obj.magic. It will be 
     possible to eliminate all of them once we have GADTs for bigarray 
     kinds. http://caml.inria.fr/mantis/view.php?id=6064 *) 

  (* Scalar types *) 
  
  type scalar_type = 
    [ `UInt8 | `Int8 | `UInt16 | `Int16 | `UInt32 | `Int32 
    | `Float32 | `Float64 ] 

  let scalar_type_byte_count = function 
  | `UInt8 | `Int8 -> 1
  | `UInt16 | `Int16 -> 2
  | `UInt32 | `Int32 | `Float32 -> 4
  | `Float64 -> 8

  let scalar_type_to_string = function 
  | `UInt8 -> "uint8" | `Int8 -> "int8" 
  | `UInt16 -> "uint16" | `Int16 -> "int16" 
  | `UInt32 -> "uint32" | `Int32 -> "int32" 
  | `Float32 -> "float32" | `Float64 -> "float64" 
    
  let pp_scalar_type ppf st = pp ppf "%s" (scalar_type_to_string st)

  let scalar_type_of_bigarray_kind : 
    ?unsigned:bool -> ('a, 'b) Bigarray.kind -> scalar_type option = 
    fun ?(unsigned = false) k -> 
      let open Bigarray in
      match Obj.magic k (* FIXME *) with 
      | k when k = int8_unsigned -> Some `UInt8
      | k when k = int8_signed -> Some `Int8
      | k when k = int16_unsigned -> Some `UInt16
      | k when k = int16_signed -> Some `Int16
      | k when k = int32 && unsigned -> Some `UInt32
      | k when k = int32 -> Some `Int32
      | k when k = float32 -> Some `Float32
      | k when k = float64 -> Some `Float64
      | _ -> None
        
  (* Buffers *) 

  type usage = 
    [ `Static_draw | `Static_read | `Static_copy
    | `Stream_draw | `Stream_read | `Stream_copy
    | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]

  let pp_usage ppf u = pp ppf begin match u with
    | `Static_draw -> "static_draw"
    | `Static_read -> "static_read"
    | `Static_copy -> "static_copy"
    | `Stream_draw -> "stream_draw"
    | `Stream_read -> "stream_read"
    | `Stream_copy -> "stream_copy"
    | `Dynamic_draw -> "dynamic_draw"
    | `Dynamic_read -> "dynamic_read"
    | `Dynamic_copy -> "dynamic_copy"
    end
      
  type bigarray_any = Ba : ('a, 'b) bigarray -> bigarray_any

  let create_bigarray_any scalar_type count = match scalar_type with
  | `UInt8 -> Ba (Ba.create Bigarray.int8_unsigned count)
  | `Int8 -> Ba (Ba.create Bigarray.int8_signed count) 
  | `UInt16 -> Ba (Ba.create Bigarray.int16_unsigned count)
  | `Int16 -> Ba (Ba.create Bigarray.int16_signed count)
  | `UInt32 -> Ba (Ba.create Bigarray.int32 count)
  | `Int32 -> Ba (Ba.create Bigarray.int32 count)
  | `Float32 -> Ba (Ba.create Bigarray.float32 count)
  | `Float64 -> Ba (Ba.create Bigarray.float64 count)

  type ('a, 'b) init = 
    [ `Cpu of scalar_type * int
    | `Bigarray of ('a, 'b) bigarray
    | `Gpu of scalar_type * int ]

  type t = 
    { usage : usage; 
      scalar_type : scalar_type;
      mutable gpu_count : int;     
      mutable gpu_exists : bool;   
      mutable gpu_upload : bool;   
      mutable cpu_autorelease : bool;
      mutable cpu : bigarray_any option; 
      mutable info : Info.t; }
    
  let create ?(unsigned = false) ?(cpu_autorelease = true) 
      ?(usage = `Static_draw) init = 
    match init with 
    | `Cpu (scalar_type, cpu_count) -> 
        let cpu = Some (create_bigarray_any scalar_type cpu_count) in
        { usage; scalar_type;
          gpu_count = 0; gpu_exists = false; gpu_upload = true; 
          cpu_autorelease; cpu; info = Info.none }
    | `Bigarray ba -> 
        let kind = Bigarray.Array1.kind ba in 
        let cpu = Some (Ba ba) in
        begin match scalar_type_of_bigarray_kind ~unsigned kind with
        | None -> invalid_arg (err_ba_kind kind)
        | Some scalar_type ->
            { usage; scalar_type; 
              gpu_count = 0; gpu_exists = false; gpu_upload = true;
              cpu_autorelease; cpu; info = Info.none }
        end
    | `Gpu (scalar_type, gpu_count) -> 
        { usage; scalar_type; 
          gpu_count = 0; gpu_exists = false; gpu_upload = false; 
          cpu_autorelease; cpu = None; info = Info.none }
        
  let usage b = b.usage 
  let scalar_type b = b.scalar_type

  let gpu_count b = b.gpu_count 
  let set_gpu_count b count = b.gpu_count <- count 
  let gpu_exists b = b.gpu_exists
  let set_gpu_exists b e = b.gpu_exists <- e
  let gpu_upload b = b.gpu_upload
  let set_gpu_upload b u = b.gpu_upload <- u

  let cpu_count b = match b.cpu with 
  | None -> 0 | Some (Ba ba) -> Bigarray.Array1.dim ba 

  let cpu_exists b = b.cpu <> None
  let cpu_byte_count b = match b.cpu with 
  | None -> 0 
  | Some (Ba ba) -> 
      Bigarray.Array1.dim ba * (scalar_type_byte_count b.scalar_type)

  let cpu : t -> ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray option = 
    fun b k' -> match b.cpu with
    | None -> None
    | Some (Ba ba) ->
        (* FIXME *) 
        let k = Bigarray.Array1.kind ba in
        if k = Obj.magic k' then Obj.magic (Some ba) else 
        let st = scalar_type_to_string b.scalar_type in 
        invalid_arg (err_ba_kind_mismatch st)

  let cpu_p b = b.cpu
  let check_kind b k = 
    let mismatch () = 
      let st = scalar_type_to_string b.scalar_type in
      invalid_arg (err_ba_kind_mismatch st) 
    in
    let unsigned = b.scalar_type = `UInt32 in
    match scalar_type_of_bigarray_kind ~unsigned k with 
    | None -> mismatch () 
    | Some st -> if b.scalar_type <> st then mismatch ()

  let set_cpu b = function 
  | None -> b.cpu <- None
  | Some ba -> 
      check_kind b (Bigarray.Array1.kind ba); 
      b.cpu <- Some (Ba ba)

  let cpu_autorelease b = b.cpu_autorelease
  let set_cpu_autorelease b bool = b.cpu_autorelease <- bool
      
  let pp ppf b = 
    let gpu = if b.gpu_exists then (str "%d" b.gpu_count) else "none" in 
    let cpu = if b.cpu <> None then (str "%d" (cpu_count b)) else "none" in
    pp ppf "@[<1><buf %a %s/%s (gpu/cpu) %a>@]" 
      pp_scalar_type b.scalar_type gpu cpu pp_usage b.usage 

  (* Renderer info *) 

  let info b = b.info 
  let set_info b i = b.info <- i
end

(* Attributes *)

module Attr = struct
  
  type t = 
    { name : string; 
      dim : int; 
      buf : Buf.t; 
      stride : int; 
      first : int; 
      normalize : bool; }

  let create ?(normalize = false) ?stride ?(first = 0) name ~dim buf = 
    let stride = match stride with None -> dim | Some stride -> stride in
    if dim < 1 || dim > 4 then invalid_arg (err_attr_dim dim) else
    { name; dim; buf; stride; first; normalize; }
                                                                 
  let name a = a.name 
  let dim a = a.dim
  let buf a = a.buf
  let stride a = a.stride 
  let first a = a.first 
  let normalize a = a.normalize 
  let rename a name = { a with name } 
  let pp ppf a =
    pp ppf "@[<1><attr %s@ %d %a@ @@%d@ +%d>@]"
      a.name a.dim Buf.pp_scalar_type a.buf.Buf.scalar_type a.first a.stride

  (* Standard attributes names *) 

  let vertex = "vertex"
  let normal = "normal" 
  let color = "color"
end

(* Primitives *) 

module Prim = struct 

  (* Primitive kinds *) 
  
  type kind = 
    [ `Points 
    | `Lines | `Line_strip | `Line_loop | `Lines_adjacency 
    | `Line_strip_adjacency 
    | `Triangles | `Triangle_strip | `Triangle_fan
    | `Triangles_adjacency | `Triangle_strip_adjacency ]
    
  let kind_to_string = function
  | `Points -> "points"
  | `Lines -> "lines"
  | `Line_strip -> "line_strip"
  | `Line_loop -> "line_loop"
  | `Lines_adjacency -> "lines_adjacency"
  | `Line_strip_adjacency -> "line_strip_adjacency"
  | `Triangles -> "triangles"
  | `Triangle_strip -> "triangle_strip"
  | `Triangle_fan -> "triangle_fan"
  | `Triangles_adjacency -> "triangles_adjacency"
  | `Triangle_strip_adjacency -> "triangle_strip_adjacency"

  let pp_kind ppf kind = pp ppf "%s" (kind_to_string kind)
 
  (* Primitives *) 
      
  module Smap = Map.Make(String) 
      
  type t = 
    { tr : M4.t; 
      name : string;
      first : int;
      count : int option; 
      index : Buf.t option;
      kind : kind; 
      attrs : Attr.t Smap.t; 
      mutable info : Info.t; }

  let gen_name =
    let count = ref 0 in 
    fun () -> incr count; Printf.sprintf "prim%d" !count
        
  let create ?(tr = M4.id) ?(name = gen_name ()) ?(first = 0) ?count ?index 
      kind attrs =
    begin match index with 
    | None -> if count = None then invalid_arg err_prim_underspec else ()
    | Some b -> 
        begin match Buf.scalar_type b with 
        | `UInt8 | `UInt16 | `UInt32 -> ()
        | `Int8 | `Int16 | `Int32 | `Float32 | `Float64 as st -> 
            invalid_arg (err_prim_not_uint (Buf.scalar_type_to_string st))
        end
    end;
    let add_attr acc a = 
      let name = a.Attr.name in
      if Smap.mem name acc then invalid_arg (err_prim_attr_dup name) else 
      Smap.add name a acc
    in
    { tr; name; first; count; index; kind;
      attrs = List.fold_left add_attr Smap.empty attrs;
      info = Info.none }

  let kind p = p.kind
  let name p = p.name 
  let index p = p.index
  let first p = p.first
  let count p = p.count
  let count_now p = match p.count with 
  | Some count -> count
  | None ->
      match p.index with
      | None -> assert false 
      | Some b ->
          if Buf.gpu_upload b then Buf.cpu_count b - p.first else 
          Buf.gpu_count b - p.first

  let tr p = p.tr

  (* Attributes *) 
  
  let attrs p = Smap.fold (fun _ v acc -> v :: acc) p.attrs []
  let iter f p = Smap.iter (fun k v -> f v) p.attrs 
  let fold f acc p = Smap.fold (fun _ v acc -> f acc v) p.attrs acc 

  let mem p n = Smap.mem n p.attrs 
  let find p n = try Some (Smap.find n p.attrs) with Not_found -> None
  let get p n = 
    try Smap.find n p.attrs with 
    | Not_found -> invalid_arg (err_miss_attr n)

  (* Printer *) 

  let pp ppf p =
    let pp_tr ppf tr = if tr == M4.id then () else pp ppf "@ %s" "tr:yes" in
    let pp_first ppf f = if f = 0 then () else pp ppf "@ @@%d" f in
    let pp_index ppf i = if i = None then () else pp ppf "@ %s" "idx:yes" in
    let pp_attrs ppf attrs = 
      let pp_attr ppf a = pp ppf "%s %d" a.Attr.name a.Attr.dim in
      let pp_sep ppf () = pp ppf ",@ " in
      pp ppf ",@ @[%a@]" (pp_list ~pp_sep pp_attr) attrs
    in
    pp ppf "@[<1><prim %s %a%a%a@ %dvs%a%a>@]"
      p.name pp_kind p.kind pp_tr p.tr pp_first p.first (count_now p) 
      pp_index p.index pp_attrs (attrs p)

  (* Renderer info *) 

  let info b = b.info 
  let set_info b i = b.info <- i
end

(* Textures *) 

module Tex = struct
  type wrap = [ `Repeat | `Mirrored_repeat | `Clamp_to_edge ]
  type mag_filter = [ `Linear | `Nearest ]
  type min_filter = 
    [ `Linear | `Linear_mipmap_linear | `Linear_mipmap_nearest
    | `Nearest | `Nearest_mipmap_linear | `Nearest_mipmap_nearest ]

  type kind = [ `D1 | `D2 | `D3 | `Buffer ]
  type format = 
    [ `R_UInt8 | `R_Int8 | `R_UInt8_norm | `R_Int8_norm 
    | `R_UInt16 | `R_Int16 | `R_UInt16_norm | `R_Int16_norm | `R_Float32
    | `RG_UInt8 | `RG_Int8 | `RG_UInt8_norm | `RG_Int8_norm 
    | `RG_UInt16 | `RG_Int16 | `RG_UInt16_norm | `RG_Int16_norm |`RG_Float32
    | `RGB_UInt8 | `RGB_Int8 | `RGB_UInt8_norm | `RGB_Int8_norm 
    | `RGB_UInt16 | `RGB_Int16 | `RGB_UInt16_norm | `RGB_Int16_norm  
    | `RGB_Float32
    | `RGBA_UInt8 | `RGBA_Int8 | `RGBA_UInt8_norm | `RGBA_Int8_norm 
    | `RGBA_UInt16 | `RGBA_Int16 | `RGBA_UInt16_norm | `RGBA_Int16_norm 
    | `RGBA_Float32
    | `SRGB_UInt8_norm 
    | `SRGBA_UInt8_norm
    | `D_UInt16 | `D_UInt24 | `D_Float32 
    | `D_UInt24_S_UInt8 | `D_Float32_S_UInt8 
    | `S_UInt8 ]

  type init = [ 
    | `D1 of float * Buf.t option
    | `D2 of size2 * Buf.t option
    | `D3 of size3 * Buf.t option
    | `Buffer of Buf.t ]
               
  type t = 
    { kind : kind; 
      format : format; 
      mutable size : size3;
      mutable buf : Buf.t option;
      mutable buf_autorelease : bool;
      mutable gpu_update : bool; 
      mutable wrap_s : wrap;
      mutable wrap_t : wrap;
      mutable wrap_r : wrap;
      mutable mipmaps : bool; 
      mutable min_filter : min_filter; 
      mutable mag_filter : mag_filter; 
      mutable info : Info.t; }
    
  let nil = 
    { kind = `D1; 
      format = `R_UInt8; 
      size = Size3.zero;
      buf = None; 
      buf_autorelease = true; 
      gpu_update = false;
      wrap_s = `Repeat; 
      wrap_t = `Repeat; 
      wrap_r = `Repeat; 
      mipmaps = false; 
      min_filter = `Nearest_mipmap_linear; 
      mag_filter = `Nearest;
      info = Info.none; }

  let create ?(wrap_s = `Repeat) ?(wrap_t = `Repeat) ?(wrap_r = `Repeat) 
      ?(mipmaps = false) ?(min_filter = `Nearest_mipmap_linear) 
      ?(mag_filter = `Nearest) ?buf_autorelease ~format init = 
    (* TODO buffer length checks *) 
    let kind, size, buf, default_buf_autorelease = match init with 
    | `D1 (s, b) -> `D1, Size3.v s 1. 1., b, true 
    | `D2 (s, b) -> `D2, Size3.v (Size2.w s) (Size2.h s) 1., b, true 
    | `D3 (s, b) -> `D3, s, b, true
    | `Buffer b -> `Buffer, Size3.zero, Some b, false
    in
    let buf_autorelease = match buf_autorelease with 
    | None -> default_buf_autorelease
    | Some b -> b
    in
    { kind; format; size; 
      buf; buf_autorelease;
      gpu_update = true;
      wrap_s; wrap_t; wrap_r;
      mipmaps; min_filter; mag_filter;
      info = Info.none; }
  
  let format t = t.format
  let kind t = t.kind
  let size2 t = V2.of_v3 t.size
  let size3 t = t.size
  let buf t = t.buf
  let set_buf t b = t.buf <- b
  let buf_autorelease t = t.buf_autorelease 
  let set_buf_autorelease t b = t.buf_autorelease <- b
  let gpu_update t = t.gpu_update
  let set_gpu_update t b = t.gpu_update <- b
  let wrap_s t = t.wrap_s 
  let wrap_t t = t.wrap_t
  let wrap_r t = t.wrap_r
  let mipmaps t = t.mipmaps
  let min_filter t = t.min_filter
  let mag_filter t = t.mag_filter 

  let pp ppf t = pp ppf "tex pp is TODO" 

  (* Renderer info *) 

  let info b = b.info 
  let set_info b i = b.info <- i
end

(* Uniforms *) 

module Uniform = struct

  (* Uniform values *) 

  type 'a value = 
    | Bool : bool -> bool value
    | Int : int -> int value 
    | Float : float -> float value
    | V2 : v2 -> v2 value 
    | V3 : v3 -> v3 value 
    | V4 : v4 -> v4 value 
    | M2 : m2 -> m2 value
    | M3 : m3 -> m3 value
    | M4 : m4 -> m4 value
    | Tex : Tex.t -> Tex.t value
    | Model_to_world : m4 value
    | Model_to_view : m4 value 
    | Model_to_clip : m4 value 
    | Model_normal_to_view : m3 value 
    | World_to_view : m4 value
    | World_to_clip : m4 value 
    | View_to_clip : m4 value 
    | Viewport_o : v2 value
    | Viewport_size : v2 value

  type builtin = 
    [ `Model_to_world 
    | `Model_to_view 
    | `Model_to_clip 
    | `Model_normal_to_view 
    | `World_to_view 
    | `World_to_clip 
    | `View_to_clip
    | `Viewport_o
    | `Viewport_size ]

  let pp_builtin ppf b = pp ppf begin match b with 
    | `Model_to_world -> "model_to_world"
    | `Model_to_view -> "model_to_view"
    | `Model_to_clip -> "model_to_clip"
    | `Model_normal_to_view -> "model_normal_to_view"
    | `World_to_view -> "world_to_view" 
    | `World_to_clip -> "world_to_clip"
    | `View_to_clip -> "view_to_clip"
    | `Viewport_o -> "viewport_o"
    | `Viewport_size -> "viewport_size"
    end

  type value_untyped = 
    [ `Bool of bool 
    | `Int of int 
    | `Float of float 
    | `V2 of v2 
    | `V3 of v3 
    | `V4 of v4 
    | `M2 of m2
    | `M3 of m3
    | `M4 of m4 
    | `Tex of Tex.t
    | `Builtin of builtin ]

  let untype : type a. a value -> value_untyped = function
  | Bool b -> `Bool b
  | Int i -> `Int i
  | Float f -> `Float f
  | V2 v -> `V2 v
  | V3 v -> `V3 v
  | V4 v -> `V4 v
  | M2 m -> `M2 m
  | M3 m -> `M3 m
  | M4 m -> `M4 m
  | Tex t -> `Tex t 
  | Model_to_world -> `Builtin `Model_to_world
  | Model_to_view -> `Builtin `Model_to_view
  | Model_to_clip -> `Builtin `Model_to_clip
  | Model_normal_to_view -> `Builtin `Model_normal_to_view
  | World_to_view -> `Builtin `World_to_view
  | World_to_clip -> `Builtin `World_to_clip
  | View_to_clip -> `Builtin `View_to_clip
  | Viewport_o -> `Builtin `Viewport_o
  | Viewport_size -> `Builtin `Viewport_size
                       
  let untype_fun : type a. a value -> (a -> value_untyped) = function
  | Bool _ -> fun v -> `Bool v
  | Int _ -> fun v -> `Int v
  | Float _ -> fun v -> `Float v
  | V2 _ -> fun v -> `V2 v
  | V3 _ -> fun v -> `V3 v
  | V4 _ -> fun v -> `V4 v
  | M2 _ -> fun v -> `M2 v
  | M3 _ -> fun v -> `M3 v
  | M4 _ -> fun v -> `M4 v
  | Tex _ -> fun v -> `Tex v 
  | Model_to_world -> fun v -> `M4 v
  | Model_to_view -> fun v -> `M4 v
  | Model_to_clip -> fun v -> `M4 v
  | Model_normal_to_view -> fun v -> `M3 v
  | World_to_view -> fun v -> `M4 v
  | World_to_clip -> fun v -> `M4 v
  | View_to_clip -> fun v -> `M4 v
  | Viewport_o -> fun v -> `V2 v 
  | Viewport_size -> fun v -> `V2 v 
      
  let pp_value_untyped ppf = function 
  | `Bool b -> Format.pp_print_bool ppf b
  | `Int i -> Format.pp_print_int ppf i 
  | `Float f -> Format.pp_print_float ppf f
  | `V2 v -> V2.pp ppf v
  | `V3 v -> V3.pp ppf v
  | `V4 v -> V4.pp ppf v
  | `M2 m -> M2.pp ppf m
  | `M3 m -> M3.pp ppf m
  | `M4 m -> M4.pp ppf m
  | `Tex t -> Tex.pp ppf t
  | `Builtin b -> pp_builtin ppf b

  (* Uniforms *) 

  type 'a t = string * 'a value * ('a -> value_untyped) 

  let u name v = name, v, (untype_fun v)
  let name (n, _, _) = n 
  
  let value : type a. a t -> a = fun (n, v, _) -> match v with 
  | Bool v -> v | Int v -> v | Float v -> v 
  | V2 v -> v | V3 v -> v | V4 v -> v 
  | M2 v -> v | M3 v -> v | M4 v -> v 
  | Tex v -> v 
  | Model_to_world -> M4.zero
  | Model_to_view -> M4.zero
  | Model_to_clip -> M4.zero
  | Model_normal_to_view -> M3.zero
  | World_to_view -> M4.zero
  | World_to_clip -> M4.zero
  | View_to_clip -> M4.zero
  | Viewport_o -> P2.o
  | Viewport_size -> V2.zero

  let set_value : type a. a t -> a -> a t = fun (n, t, inj) v -> match t with 
  | Bool _ -> (n, Bool v, inj)
  | Int _ -> (n, Int v, inj)
  | Float _ -> (n, Float v, inj)
  | V2 _ -> (n, V2 v, inj)
  | V3 _ -> (n, V3 v, inj)
  | V4 _ -> (n, V4 v, inj)
  | M2 _ -> (n, M2 v, inj)
  | M3 _ -> (n, M3 v, inj)
  | M4 _ -> (n, M4 v, inj)
  | Tex _ -> (n, Tex v, inj)
  | Model_to_world -> (n, M4 v, inj)
  | Model_to_view -> (n, M4 v, inj)
  | Model_to_clip -> (n, M4 v, inj)
  | Model_normal_to_view -> (n, M3 v, inj)
  | World_to_view -> (n, M4 v, inj)
  | World_to_clip -> (n, M4 v, inj)
  | View_to_clip -> (n, M4 v, inj)
  | Viewport_o -> (n, V2 v, inj)
  | Viewport_size -> (n, V2 v, inj)

  let v = set_value 

  let set_to_model_to_world (n, t, inj) = (n, Model_to_world, inj)

  let is_value_builtin : type a. a t -> bool = fun (_, v, _) -> match v with 
  | Model_to_world -> true 
  | Model_to_view -> true 
  | Model_to_clip -> true 
  | Model_normal_to_view -> true
  | World_to_view -> true 
  | World_to_clip -> true
  | View_to_clip -> true
  | Viewport_o -> true
  | Viewport_size -> true 
  | Bool _ -> false | Int _ -> false | Float _ -> false 
  | V2 _ -> false | V3 _ -> false | V4 _ -> false 
  | M2 _ -> false | M3 _ -> false | M4 _ -> false
  | Tex _ -> false

  let pp ppf (n, v, _) = pp ppf "@[<1>%s =@ %a]" n pp_value_untyped (untype v)

  let bool n v = let v = Bool v in (n, v, untype_fun v)
  let int n v = let v = Int v in (n, v, untype_fun v)
  let float n v = let v = Float v in (n, v, untype_fun v)
  let v2 n v = let v = V2 v in (n, v, untype_fun v)
  let v3 n v = let v = V3 v in (n, v, untype_fun v)
  let v4 n v = let v = V4 v in (n, v, untype_fun v)
  let m2 n v = let v = M2 v in (n, v, untype_fun v)
  let m3 n v = let v = M3 v in (n, v, untype_fun v)
  let m4 n v = let v = M4 v in (n, v, untype_fun v)
  let tex n v = let v = Tex v in (n, v, untype_fun v)
  let model_to_world n = let v = Model_to_world in (n, v, untype_fun v)
  let model_to_clip n = let v = Model_to_clip in (n, v, untype_fun v)
  let model_to_view n = let v = Model_to_view in (n, v, untype_fun v)
  let model_normal_to_view n = 
    let v = Model_normal_to_view in (n, v, untype_fun v)

  let world_to_view n = let v = World_to_view in (n, v, untype_fun v)
  let world_to_clip n = let v = World_to_clip in (n, v, untype_fun v)
  let view_to_clip n = let v = View_to_clip in (n, v, untype_fun v)
  let viewport_o n = let v = Viewport_o in (n, v, untype_fun v)
  let viewport_size n = let v = Viewport_size in (n, v, untype_fun v)

  (* Uniform sets *)

  module Smap = Map.Make(String) 

  type set = value_untyped Smap.t 
    
  let empty = Smap.empty 
  let is_empty = Smap.is_empty
  let add s (n, v, _) = Smap.add n (untype v) s   
  let ( + ) = add
  let set s u = add s u, u
  let def s (n, v, inj) v = Smap.add n (inj v) s
  let def_v s (n, _, _) v = Smap.add n (untype v) s
  let def_named s n v = Smap.add n v s
  let mem_named s n = Smap.mem n s 
  let find s (n, _, _) = try Some (Smap.find n s) with Not_found -> None
  let find_named s n = try Some (Smap.find n s) with Not_found -> None
  let get s (n, _, _) = try Smap.find n s with 
  | Not_found -> invalid_arg (err_miss_uniform n)

  let get_named s n = try Smap.find n s with 
  | Not_found -> invalid_arg (err_miss_uniform n)

  let fold f acc s =
    let f' k v acc = f acc k v in 
    Smap.fold f' s acc 

  let pp_set ppf s = failwith "TODO"
end


(* Programs *) 

module Prog = struct

  (* Source locations. *) 

  type loc = [ `Loc of string * int | `Unknown ] 

  let pp_loc ppf = function 
  | `Unknown -> pp ppf "????:??" 
  | `Loc (f, l) -> pp ppf "%s:%d" f l

  let parse_loc stack =                                          (* Grrrrr. *) 
    try
      let start = String.index stack '\n' in 
      let fstart = String.index_from stack start '\"' + 1 in 
      let fend = String.rindex stack '\"' - 1 in
      let file = String.sub stack fstart (fend - fstart + 1) in
      let lstart = fend + 9 in
      let lend = String.rindex stack ',' - 1 in
      let line = String.sub stack lstart (lend - lstart + 1) in
      `Loc (file, int_of_string line - 1)
    with 
    | Not_found | Failure _ -> `Unknown
  
  (* Inserts *) 

  type insert = loc * string 

  let insert ?loc src = 
    let stack = Printexc.get_callstack 2 in
    let loc = match loc with 
    | None -> parse_loc (Printexc.raw_backtrace_to_string stack)
    | Some loc -> loc 
    in
    loc, src

  (* Shaders *) 

  type lang = [ `GLSL of int | `GLSL_ES of int ] 

  type shader_kind = 
    [ `Vertex | `Tess_control | `Tess_evaluation | `Geometry
    | `Fragment | `Compute ]

  let pp_shader_kind ppf sk = pp ppf begin match sk with 
    | `Vertex -> "vertex"
    | `Tess_control -> "tess_control"
    | `Tess_evaluation -> "tess_evaluation"
    | `Geometry -> "geometry"
    | `Fragment -> "fragment"
    | `Compute -> "compute"
    end

  type shader = 
    { kind : shader_kind; 
      lang : lang option;
      srcs : (loc * string) list (* list is reversed *); }

  let shader ?lang ?loc ?(inserts = []) kind src =
    let stack = Printexc.get_callstack 2 in
    let loc = match loc with
    | None -> parse_loc (Printexc.raw_backtrace_to_string stack)
    | Some loc -> loc 
    in
    { kind; lang; srcs = (loc, src) :: (List.rev inserts) }

  let kind s = s.kind
  let loc s = fst (List.hd s.srcs) 
  let lang s = s.lang

  type source = string * (int * string) list 
  
  let source ?lang s =
    let lang = match lang with None -> s.lang | Some _ as lang -> lang in
    let version = match lang with
    | Some (`GLSL v | `GLSL_ES v) -> str "#version %d" v
    | None -> ""
    in
    let add_loc (file_id, map, srcs) (loc, src) = match loc with
    | `Unknown -> file_id, map, ("#line 0 0" :: src :: srcs)
    | `Loc (f, l) -> 
        let file_id = file_id + 1 in 
        let map = (file_id, f) :: map in 
        file_id, map, (str "#line %d %d" l file_id) :: src :: srcs
    in
    let start = (0, [], []) in          (* file_id 0 is used for `Unknown *) 
    let _, map, srcs = List.fold_left add_loc start s.srcs in
    String.concat "\n" (version :: srcs), map
    
  (* Programs *) 

  let gen_name =
    let count = ref 0 in 
    fun () -> incr count; Printf.sprintf "prog%d" !count

  type t = 
    { name : string; 
      shaders : shader list; 
      uset : Uniform.set;
      mutable info : Info.t }
      
  let create ?(name = gen_name ()) ?(uset = Uniform.empty) shaders = 
    { name; shaders; uset; info = Info.none }

  let name p = p.name
  let uniforms p = p.uset
  let shaders p = p.shaders

  (* Renderer info *) 

  let info e = e.info 
  let set_info e i = e.info <- i
end

module View = struct

  (* View and projection matrices *)

  type fov = [ `H of float | `V of float ]

  let persp ~fov ~aspect ~near ~far = 
    let half_w, half_h = match fov with 
    | `H theta -> 
        let half_w = near *. tan (0.5 *. theta) in 
        half_w, half_w /. aspect
    | `V theta -> 
        let half_h = near *. tan (0.5 *. theta) in 
        aspect *. half_h, half_h 
    in
    Gg.M4.persp 
      ~left:(-.half_w) ~right:(half_w)
      ~bottom:(-.half_h) ~top:(half_h)
      ~near ~far

  let look ?(up = V3.oy) ~at ~from:pos () = 
    let oz' = V3.(unit @@ pos - at) in
    let ox' = V3.(unit @@ cross up oz') in 
    let oy' = V3.(unit @@ cross oz' ox') in 
    let move = V3.neg pos in M4.v
      (V3.x ox') (V3.y ox') (V3.z ox') (V3.dot ox' move)
      (V3.x oy') (V3.y oy') (V3.z oy') (V3.dot oy' move)
      (V3.x oz') (V3.y oz') (V3.z oz') (V3.dot oz' move)
      0.          0.        0.         1.       

  (* View *) 

  type t = { mutable tr : m4; mutable proj : m4; mutable viewport : box2 }

  let create ?(tr = M4.id) ?(proj = persp (`H Float.pi_div_4) 1.5 1. 100.) 
      ?(viewport = Box2.unit) () = { tr; proj; viewport }
  
  let tr v = v.tr
  let set_tr v m = v.tr <- m
  let proj v = v.proj
  let set_proj v m = v.proj <- m
  let viewport v = v.viewport 
  let set_viewport v b = v.viewport <- b


  (* Coordinate system transforms *) 
      

  let viewport_of_surface view nsc =
    V2.(div (nsc - Box2.o view.viewport) (Box2.size view.viewport))
      
  let viewport_of_ndc view ndc =
    V2.(0.5 * (ndc + Size2.unit))

  let surface_of_viewport view nvpc =
    V2.((Box2.o view.viewport) + (mul nvpc (Box2.size view.viewport)))
                              
  let surface_of_ndc view ndc =
    let nvpc = V2.(0.5 * (ndc + Size2.unit)) in
    surface_of_viewport view nvpc

  let ndc_of_viewport view nvpc = 
    V2.(2. * nvpc - Size2.unit)

  let ndc_of_surface view nsc =
    let nvpc = viewport_of_surface view nsc in
    ndc_of_viewport view nvpc
end

module Effect = struct  

  (* Rasterization state *) 
 
  type cull = [ `Front | `Back ] 
  type raster = { raster_cull : cull option } 
  let default_raster = { raster_cull = None } 

  (* Depth state *) 

  type depth_test = 
    [ `Never | `Less | `Equal | `Lequal | `Greater | `Nequal 
    | `Gequal | `Always ]

  type depth = { depth_test : depth_test option; depth_write : bool;
                 depth_offset : float * float; 
                 (** factor, units, see glPolygonOffset *) }
  (** The type for depth state *) 

  let default_depth = { depth_test = Some `Less; depth_write = true; 
                        depth_offset = (0., 0.) }


  type t = 
    { raster : raster; 
      depth : depth;
      prog : Prog.t; 
      mutable uniforms : Uniform.set; 
      mutable info : Info.t; } 

  let create ?(raster = default_raster) ?(depth = default_depth) prog =
    let uniforms = Prog.uniforms prog in 
    { raster; depth; prog; uniforms; info = Info.none }
           
  let prog e = e.prog
  let uniforms e = e.uniforms 
  let get_uniform e u = Uniform.get e.uniforms u 
  let set_uniform e u v = e.uniforms <- Uniform.def e.uniforms u v
  let raster e = e.raster
  let depth e = e.depth
  
  (* Renderer info *) 

  let info e = e.info
  let set_info e i = e.info <- i
end

type op = { count : int; effect : Effect.t; uniforms : Uniform.set; 
            tr : m4; prim : Prim.t }

let op ?(count = 1) ?(uniforms = Uniform.empty) ?(tr = M4.id) effect prim = 
  { count; effect; uniforms; tr; prim }

module Renderer = struct

  module Log = struct
    
    let split_string sep s =                                (* damned... *) 
      let rec split accum j = 
        let i = try (String.rindex_from s j sep) with Not_found -> -1 in
        if (i = -1) then 
          let p = String.sub s 0 (j + 1) in 
          if p <> "" then p :: accum else accum
        else 
        let p = String.sub s (i + 1) (j - i) in
        let accum' = if p <> "" then p :: accum else accum in
        split accum' (i - 1)
      in
      split [] (String.length s - 1)
        
    let lines s = split_string '\n' s 

    (* Compiler messages *) 

    type compiler_msg = 
      [ `Msg of string | `Msg_loc of string * Prog.loc * string ]

    let pp_compiler_msg ppf = function 
    |  `Msg m -> pp ppf "@[%s@]" m 
    |  `Msg_loc (t, loc, m) -> pp ppf "@[%s:%a: %s@]" t Prog.pp_loc loc m

    type compiler_msg_parser = string -> 
      [ `Loc of string * int * int * string | `Unparsed of string  ]
    
    let compiler_msg_parser_default l =
      let locify l pre file line rest = 
        try
          let file = int_of_string (String.trim file) in 
          let line = int_of_string (String.trim line) in 
          `Loc (pre, file, line, String.concat ":" rest) 
        with Failure _ -> `Unparsed l 
      in
      match split_string ':' l with 
      | pre :: file :: line :: rest -> locify l pre file line rest 
      | file :: line :: rest -> locify l "" file line rest 
      | _ -> `Unparsed l

    let compiler_msg_parser_raw s = `Unparsed s

    let compiler_msg log parser file_id_map = 
      let lines = lines log in 
      let parse l = match parser l with 
      | `Unparsed m -> `Msg m 
      | `Loc (s0, fid, line, s1) -> 
          let loc = try `Loc (List.assoc fid file_id_map, line) with 
          | Not_found -> `Unknown 
          in
          `Msg_loc (s0, loc, s1) 
      in
      `Compiler (List.rev (List.rev_map parse lines))

    (* Renderer messages *) 

    type msg = 
      [ `Compiler of compiler_msg list
      | `Linker of string list 
      | `Missing_attr of Prim.t * string 
      | `Unsupported_shaders of Prog.t * Prog.shader list 
      | `Msg of string ]

    let pp_msg ppf (msg : msg) = match msg with
    | `Compiler msgs -> pp ppf "@[<v>%a@]" (pp_list pp_compiler_msg) msgs 
    | `Linker msgs -> pp ppf "@[<v>%a@]" (pp_list pp_str) msgs
    | `Msg m -> pp ppf "@[%s@]" m 
    | `Missing_attr (p, aname) -> 
        pp ppf "@[Primitive %s: missing@ %s attribute@]" 
          (Prim.name p) aname 
    | `Unsupported_shaders (p, sl) -> 
        let pp_unsup ppf s =
          pp ppf "%a: %a shader unsupported (program %s)" 
            Prog.pp_loc (Prog.loc s) Prog.pp_shader_kind (Prog.kind s) 
            (Prog.name p)
        in
        pp ppf "@[<v>%a@]" (pp_list pp_unsup) sl

    (* Logs *) 

    type level = [ `Error | `Debug ] 
    type t = level -> msg -> unit

    let of_formatter ppf level msg = pp ppf "%a@." pp_msg msg 
  end

  (* TODO remove that ? *) 
  module Stat = struct
    type t = 
      { mutable ops : int;
        mutable max_ops : int;
        mutable vertices : int; 
        mutable max_vertices : int;
        mutable faces : int; 
        mutable max_faces : int; }
      
    let stats = 
      { ops = 0; 
        max_ops = 0; 
        vertices = 0; 
        max_vertices = 0; 
        faces = 0; 
        max_faces = 0; }
      
    let begin_stats stats now =
      stats.ops <- 0;
      stats.vertices <- 0;
      stats.faces <- 0
        
    let end_stats s now = 
      if s.ops > s.max_ops then s.max_ops <- s.ops;
      if s.vertices > s.max_vertices then s.max_vertices <- s.vertices; 
      if s.faces > s.max_faces then s.max_faces <- s.faces
  end
  
  type clears = 
    { clear_color : color option; 
      clear_depth : float option; 
      clear_stencil : int option; }

  let default_clears = 
    { clear_color = Some Color.black;
      clear_depth = Some 1.; 
      clear_stencil = None; }
                         
  module type T = sig
    type t 
    val name : string
    val create : 
      ?compiler_msg_parser:Log.compiler_msg_parser -> Log.t -> debug:bool -> 
      size2 -> t
    val size : t -> size2
    val set_size : t -> size2 -> unit
    val view : t -> View.t 
    val set_view : t -> View.t -> unit
    val clears : t -> clears 
    val set_clears : t -> clears -> unit
    val add_op : t -> op -> unit
    val render : t -> clear:bool -> unit
    val release : t -> unit

    module Cap : sig
      val shader_kinds : t -> Prog.shader_kind list
      val gl_version : t ->
        [ `GL of (int * int * int) | `GLES of (int * int * int) | `Unknown ] 
      val glsl_version : t ->
        [ `GL of (int * int * int) | `GLES of (int * int * int) | `Unknown ] 
      val gl_renderer : t -> string 
      val gl_vendor : t -> string 
    end

    module Buf : sig
      val map : t -> [ `R | `W | `RW ]  -> Buf.t -> 
        ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray 
      val unmap : t -> Buf.t -> unit
    end
  end 

  type t = R : (module T with type t = 'a) * 'a -> t
    
  let stdlog = Log.of_formatter Format.err_formatter 
  let create ?compiler_msg_parser ?(log = stdlog) ?(debug = false) ~size 
      backend =
    let module R = (val backend : T) in
    let r = R.create ?compiler_msg_parser log ~debug size in
    R ((module R), r)
                                 
  let size (R ((module R), r)) = R.size r
  let set_size (R ((module R), r)) size = R.set_size r size
  let view (R ((module R), r)) = R.view r 
  let set_view (R ((module R), r)) v = R.set_view r v
  let clears (R ((module R), r)) = R.clears r 
  let set_clears (R ((module R), r)) clears = R.set_clears r clears
  let add_op (R ((module R), r)) op = R.add_op r op
  let render ?(clear = true) (R ((module R), r)) = R.render r ~clear
  let release (R ((module R), r)) = R.release r

  module Cap = struct

    let shader_kinds (R ((module R), r)) = R.Cap.shader_kinds r

    (* OpenGL implementation information *) 

    type gl_version =
      [ `GL of (int * int * int) | `GLES of (int * int * int) | `Unknown ] 

    let pp_gl_version ppf v = 
      let pp_version ppf (x, y, z) = 
        if z = 0 then pp ppf "%d.%d" x y else pp ppf "%d.%d.%d" x y z 
      in
      match v with 
      | `GL (x, y, z) -> pp_version ppf (x, y, z) 
      | `GLES (x, y, z) -> pp ppf "@[ES %a@]" pp_version (x, y, z) 
      | `Unknown -> pp ppf "unknown" 

    let parse_version s = 
      let s = String.trim s in
      let v = try String.sub s 0 (String.index s ' ') with Not_found -> s in 
      let int s = int_of_string (String.trim s) in
      try match List.map int (Log.split_string '.' v) with
      | [x; y; z] -> Some (x, y, z) 
      | [x; y] -> Some (x, y, 0)
      | _ -> None
      with Failure _ -> None
                      
    let gl_version (R ((module R), r)) = R.Cap.gl_version r
    let glsl_version (R ((module R), r)) = R.Cap.glsl_version r
    let gl_renderer (R ((module R), r)) = R.Cap.gl_renderer r
    let gl_vendor (R ((module R), r)) = R.Cap.gl_vendor r

    let pp_gl_synopsis ppf r = 
      pp ppf "@[Renderer %s@ -- OpenGL %a / GLSL %a@]" 
        (gl_renderer r) 
        pp_gl_version (gl_version r) 
        pp_gl_version (glsl_version r)
  end

  module Private = struct    
    module Id = Id
    module Info = Info
    module Cap = Cap 
    module Buf = Buf
    module Attr = Attr
    module Prim = Prim
    module Tex = Tex
    module Prog = Prog
    module Effect = Effect
    module Log = Log
  end

  module Buf = struct
    type access = [ `R | `W | `RW ]
    let map (R ((module R), r)) m buf k = R.Buf.map r m buf k 
    let unmap (R ((module R), r)) buf = R.Buf.unmap r buf
  end
end

type buf = Buf.t
type attr = Attr.t
type prim = Prim.t
type tex = Tex.t
type 'a uniform = 'a Uniform.t
type prog = Prog.t
type effect = Effect.t
type view = View.t
type renderer = Renderer.t

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
