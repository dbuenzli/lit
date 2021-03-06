(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Gg

let str = Format.asprintf
let pp = Format.fprintf
let pp_str = Format.pp_print_string
let rec pp_list ?(pp_sep = Format.pp_print_cut) pp_v ppf = function 
| [] -> ()
| v :: vs ->
    pp_v ppf v; if vs <> [] then (pp_sep ppf (); pp_list ~pp_sep pp_v ppf vs)

(* Invalid argument strings *) 

let err_miss_key n k = str "no %s %s" n k
let err_miss_attr k = str "no attribute %s" k 
let err_attr_dim dim = str "invalid attribute dimension %d" dim 
let err_no_cpu_buffer = str "no cpu buffer" 

let err_ba_kind st = 
  str "bigarray kind not compatible with scalar type %a" Ba.pp_scalar_type st

let err_st_mismatch exp fnd = 
  str "requested scalar type %a found %a" 
    Ba.pp_scalar_type exp Ba.pp_scalar_type fnd

let err_raster_sf_dim d = str "raster sample format dimension > 4 (%d)" d

let err_prim_underspec = str "one of ?index or ?count must be specified"
let err_prim_not_uint t = str "index's scalar type not unsigned integer (%s)" t
let err_prim_attr_dup n = str "attribute %s specified more than once" n
let err_miss_uniform k = str "no uniform %s" k
let err_neg_arg arg v = str "negative argument %s (%d)" arg v
let err_not_mstex k = str "not a multisample texture (%s) k" k
let err_mstex = str "can't specify the data of a multisample texture" 
let err_fb_read_msample = str "can't read a multisample buffer"
let err_fb_read_depth_stencil = 
  str "can only read back `Depth_stencil with an `UInt32 buffer"
    
module Smap = Map.Make(String) 

(* Backend info *) 

module BInfo = struct
  type t = exn         (* universal type, see http://mlton.org/UniversalType *) 
  let create (type s) () = 
    let module M = struct exception E of s option end in 
    (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)

  let none = fst (create ()) None
end

(* These types need to be defined here for being able to define 
   the [renderer] type, so that we can use it in the modules. *) 

(* Buffers *) 

module Buf_types = struct 

  type usage =
    [ `Static_draw | `Static_read | `Static_copy
    | `Stream_draw | `Stream_read | `Stream_copy
    | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]
    
  type ('a, 'b) init = 
    [ Gg.buffer
    | `Cpu of Ba.scalar_type * int
    | `Gpu of Ba.scalar_type * int ]

  type bigarray_any = Ba : ('a, 'b) bigarray -> bigarray_any    

  type t = 
    { usage : usage; 
      scalar_type : Ba.scalar_type;
      mutable gpu_count : int;     
      mutable gpu_exists : bool;   
      mutable gpu_upload : bool;
      mutable cpu_autorelease : bool;
      mutable cpu : bigarray_any option; 
      mutable binfo : BInfo.t; }

  type access = [ `R | `W | `RW ]
end

type buf = Buf_types.t

(* Attributes *) 

module Attr_types = struct
  type t = 
    { name : string; 
      dim : int; 
      buf : buf; 
      stride : int; 
      first : int; 
      normalize : bool; }
end

type attr = Attr_types.t

(* Primitives *) 

module Prim_types = struct
  
  type kind = 
    [ `Points 
    | `Lines | `Line_strip | `Line_loop | `Lines_adjacency 
    | `Line_strip_adjacency 
    | `Triangles | `Triangle_strip | `Triangle_fan
    | `Triangles_adjacency | `Triangle_strip_adjacency ]
            
  type t = 
    { tr : M4.t; 
      name : string;
      first : int;
      count : int option; 
      index : buf option;
      kind : kind; 
      attrs : attr Smap.t; 
      mutable binfo : BInfo.t; }
end

type prim = Prim_types.t

(* Textures *) 

module Tex_types = struct

  type wrap = [ `Repeat | `Mirrored_repeat | `Clamp_to_edge ]      
  type mag_filter = [ `Linear | `Nearest ]
  type min_filter = 
    [ `Linear | `Linear_mipmap_linear | `Linear_mipmap_nearest
    | `Nearest | `Nearest_mipmap_linear | `Nearest_mipmap_nearest ]

  type kind = [ `D1 | `D2 | `D3 | `D2_ms | `D3_ms | `Buffer ]

  type sample_format = 
    [ `D1 of Ba.scalar_type * bool 
    | `D2 of Ba.scalar_type * bool 
    | `D3 of Ba.scalar_type * bool 
    | `D4 of Ba.scalar_type * bool
    | `SRGB of [ `UInt8 ]
    | `SRGBA of [ `UInt8 ]
    | `Depth of [ `UInt16 | `UInt24 | `Float32 ]
    | `Stencil of [ `UInt8 ]
    | `Depth_stencil of [ `UInt24_UInt8 | `Float32_UInt8 ] ]

  type init =
    [ `D1 of sample_format * float * buf option
    | `D2 of sample_format * size2 * buf option
    | `D3 of sample_format * size3 * buf option
    | `D2_ms of sample_format * size2 * int * bool 
    | `D3_ms of sample_format * size3 * int * bool 
    | `Buffer of sample_format * buf ]

  type t = 
    { kind : kind; 
      sample_format : sample_format; 
      mutable size : size3;
      mutable buf : buf option;
      mutable buf_autorelease : bool;
      mutable gpu_update : bool; 
      mutable wrap_s : wrap;
      mutable wrap_t : wrap;
      mutable wrap_r : wrap;
      mutable mipmaps : bool; 
      mutable multisample : (int * bool) option;
      mutable min_filter : min_filter; 
      mutable mag_filter : mag_filter; 
      mutable binfo : BInfo.t; }  
end

type tex = Tex_types.t

(* Uniforms *) 

module Uniform_types = struct
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
    | Tex : tex -> tex value
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
    | `Tex of tex
    | `Builtin of builtin ]

  type 'a t = string * 'a value * ('a -> value_untyped) 

  type set = value_untyped Smap.t   
end

type 'a uniform = 'a Uniform_types.t

(* Programs *) 

module Prog_types = struct
  type loc = [ `Loc of string * int | `Unknown ] 
  type insert = loc * string 
  type lang = [ `GLSL of int | `GLSL_ES of int ] 
              
  type shader_stage = 
    [ `Vertex | `Tess_control | `Tess_evaluation | `Geometry
    | `Fragment | `Compute ]
    
  type shader = 
    { stage : shader_stage; 
      lang : lang option;
      srcs : (loc * string) list (* list is reversed *); }
    
  type source = string * (int * string) list 

  type t = 
    { name : string; 
      shaders : shader list; 
      uset : Uniform_types.set;
      mutable binfo : BInfo.t }
end

type prog = Prog_types.t

(* Effects *) 

module Effect_types = struct
  type raster_face_cull = [ `Front | `Back ] 
  type raster = 
    { raster_face_cull : raster_face_cull option;
      raster_multisample : bool; } 

  type depth_test = 
    [ `Never | `Less | `Equal | `Lequal | `Greater | `Nequal | `Gequal 
    | `Always ]

  type depth = 
    { depth_test : depth_test option; 
      depth_write : bool;
      depth_offset : float * float; }
    
  type blend_mul = 
    [ `Zero | `One
    | `Src | `One_minus_src
    | `Src_a | `One_minus_src_a | `Src_a_saturate
    | `Src1 | `One_minus_src1
    | `Src1_a | `One_minus_src1_a
    | `Dst | `One_minus_dst
    | `Dst_a | `One_minus_dst_a
    | `Cst | `One_minus_cst
    | `Cst_a | `One_minus_cst_a ]

  type blend_eq = 
    [ `Add of blend_mul * blend_mul 
    | `Sub of blend_mul * blend_mul 
    | `Rev_sub of blend_mul * blend_mul
    | `Min 
    | `Max ]

  type blend =  
    { blend : bool;
      blend_rgb : blend_eq;
      blend_a : blend_eq;
      blend_cst : color; }

  type t = 
    { raster : raster; 
      depth : depth;
      blend : blend;
      prog : prog; 
      mutable uniforms : Uniform_types.set; 
      mutable binfo : BInfo.t; } 
end

type effect = Effect_types.t

(* Views *) 

module View_types = struct
  type t = { tr : m4; proj : m4; viewport : box2 }
end

type view = View_types.t 

(* Framebuffers *) 

module Rbuf_types = struct
  type t = { multisample : int option; 
             size : size2; 
             sample_format : Tex_types.sample_format; 
             mutable binfo : BInfo.t }
end

module Fbuf_types = struct
    
  type image = 
    [ `Tex of int * tex 
    | `Tex_layer of int * int * tex 
    | `Rbuf of Rbuf_types.t ] 
    
  type attachement = 
    [ `Color of int * image 
    | `Depth of image 
    | `Stencil of image 
    | `Depth_stencil of image ]
    
  type clears = 
    { clear_color : color option; 
      clear_depth : float option; 
      clear_stencil : int option; }

  type t = 
    { attachements : attachement list; 
      is_multisample : bool;
      mutable clears : clears;
      mutable binfo : BInfo.t }

  type status = 
    [ `Complete
    | `Incomplete_attachement
    | `Incomplete_draw_buffer
    | `Incomplete_layer_targets
    | `Incomplete_missing_attachement
    | `Incomplete_multisample
    | `Incomplete_read_buffer
    | `Undefined
    | `Unsupported 
    | `Unknown of int ]

  type read = 
    [ `Color_r of int 
    | `Color_g of int 
    | `Color_b of int 
    | `Color_rgb of int 
    | `Color_rgba of int
    | `Depth
    | `Stencil 
    | `Depth_stencil ]

  type blit_buffer = [ `Color | `Depth | `Stencil ] 
  type blit_filter = [ `Nearest | `Linear ]
end

type fbuf = Fbuf_types.t

(* Renderer types *) 

module Log_types = struct

  type compiler_msg = 
    [ `Msg of string | `Msg_loc of string * Prog_types.loc * string ]
    
  type compiler_msg_parser = string -> 
    [ `Loc of string * int * int * string | `Unparsed of string  ]
    
  type msg = 
    [ `Compiler of compiler_msg list
    | `Linker of string list 
    | `Missing_attr of prim * string 
    | `Unsupported_shaders of prog * Prog_types.shader list 
    | `Msg of string ]
    
  type level = [ `Error | `Debug ] 
  type t = level -> msg -> unit
end

type op = 
  { count : int; 
    effect : effect; 
    uniforms : Uniform_types.set; 
    tr : m4;
    prim : prim; }

module Cap_types = struct
  type gl_version =
    [ `GL of (int * int * int) | `GLES of (int * int * int) | `Unknown ] 

  type t = 
    { c_shader_stages : Prog_types.shader_stage list; 
      c_max_samples : int;
      c_max_tex_size : int; 
      c_max_render_buffer_size : int; 
      c_gl_version : gl_version; 
      c_glsl_version : gl_version; 
      c_gl_renderer : string;
      c_gl_vendor : string; }
end

module Renderer_types = struct
     
  module type T = sig
    type t 

    module BCap : sig
      val caps : t -> Cap_types.t 
    end
      
    module BBuf : sig 
      val sync_cpu_to_gpu : t -> buf -> unit
      val sync_gpu_to_cpu : t -> buf -> unit
      val gpu_map : t -> [ `R | `W | `RW ]  -> buf -> 
        ('a, 'b) Ba.ba_scalar_type -> ('a, 'b) bigarray 
      val gpu_unmap : t -> buf -> unit
    end

    module BFbuf : sig        
      val clear : t -> fbuf -> unit
      val status : t -> fbuf -> Fbuf_types.status
      val read : t -> fbuf -> Fbuf_types.read -> 
        box2 -> first:int -> w_stride:int option -> buf -> unit
      val blit : t -> Fbuf_types.blit_filter -> Fbuf_types.blit_buffer list -> 
        src:fbuf -> box2 -> dst:fbuf -> box2 -> unit
    end

    val name : string
    val create : 
      ?compiler_msg_parser:Log_types.compiler_msg_parser -> Log_types.t ->
      debug:bool -> size2 -> t

    val size : t -> size2
    val set_size : t -> size2 -> unit
    val view : t -> view
    val set_view : t -> view -> unit
    val fbuf : t -> fbuf 
    val set_fbuf : t -> fbuf -> unit
    val add_op : t -> op -> unit
    val render : t -> clear:bool -> unit
    val release : t -> unit
  end
end

type renderer = R : (module Renderer_types.T with type t = 'a) * 'a -> renderer

(* Buffers *) 

module Buf = struct

  include Buf_types

  (* FIXME: this module has two uses of Obj.magic. It will be 
     possible to eliminate all of them once we have GADTs for bigarray 
     kinds. http://caml.inria.fr/mantis/view.php?id=6064 *) 
        
  (* Buffers *) 

  let pp_usage ppf u = pp ppf begin match u with
    | `Static_draw -> "static-draw"
    | `Static_read -> "static-read"
    | `Static_copy -> "static-copy"
    | `Stream_draw -> "stream-draw"
    | `Stream_read -> "stream-read"
    | `Stream_copy -> "stream-copy"
    | `Dynamic_draw -> "dynamic-draw"
    | `Dynamic_read -> "dynamic-read"
    | `Dynamic_copy -> "dynamic-copy"
    end
      
  let create_bigarray_any scalar_type count = match scalar_type with
  | `Int8 -> Ba (Ba.create Ba.Int8 count) 
  | `Int16 -> Ba (Ba.create Ba.Int16 count)
  | `Int32 -> Ba (Ba.create Ba.Int32 count)
  | `Int64 -> Ba (Ba.create Ba.Int64 count)
  | `UInt8 -> Ba (Ba.create Ba.UInt8 count)
  | `UInt16 -> Ba (Ba.create Ba.UInt16 count)
  | `UInt32 -> Ba (Ba.create Ba.UInt32 count)
  | `UInt64 -> Ba (Ba.create Ba.UInt64 count)
  | `Float16 -> Ba (Ba.create Ba.Float16 count)
  | `Float32 -> Ba (Ba.create Ba.Float32 count)
  | `Float64 -> Ba (Ba.create Ba.Float64 count)
    
  let create ?(cpu_autorelease = true) ?(usage = `Static_draw) init =
    let create scalar_type ?(gpu_count = 0) cpu = 
      { usage; scalar_type; 
        gpu_count; gpu_exists = false; gpu_upload = true; 
        cpu_autorelease; cpu; binfo = BInfo.none }
    in
    match init with 
    | `Cpu (scalar_type, cpu_count) -> 
        let cpu = Some (create_bigarray_any scalar_type cpu_count) in
        create scalar_type cpu
    | `Gpu (scalar_type, gpu_count) -> 
        create scalar_type ~gpu_count None 
    | `Int8 ba -> create `Int8 (Some (Ba ba)) 
    | `Int16 ba -> create `Int16 (Some (Ba ba)) 
    | `Int32 ba -> create `Int32 (Some (Ba ba)) 
    | `Int64 ba -> create `Int64 (Some (Ba ba)) 
    | `UInt8 ba -> create `UInt8 (Some (Ba ba)) 
    | `UInt16 ba -> create `UInt16 (Some (Ba ba)) 
    | `UInt32 ba -> create `UInt32 (Some (Ba ba)) 
    | `UInt64 ba -> create `UInt64 (Some (Ba ba)) 
    | `Float16 ba -> create `Float16 (Some (Ba ba)) 
    | `Float32 ba -> create `Float32 (Some (Ba ba)) 
    | `Float64 ba -> create `Float64 (Some (Ba ba))
        
  let usage b = b.usage 
  let scalar_type b = b.scalar_type

  (* GPU *) 
  
  let gpu_count b = b.gpu_count 
  let gpu_byte_count b = b.gpu_count * (Ba.scalar_type_byte_count b.scalar_type)
  let set_gpu_count b count = b.gpu_count <- count 
  let gpu_exists b = b.gpu_exists
  let set_gpu_exists b e = b.gpu_exists <- e
  let gpu_upload b = b.gpu_upload
  let set_gpu_upload b u = b.gpu_upload <- u
  let sync_gpu_to_cpu (R ((module R), r)) buf = R.BBuf.sync_gpu_to_cpu r buf
  let gpu_map (R ((module R), r)) m buf k = R.BBuf.gpu_map r m buf k 
  let gpu_unmap (R ((module R), r)) buf = R.BBuf.gpu_unmap r buf

  (* CPU *) 

  let cpu_count b = match b.cpu with 
  | None -> 0 | Some (Ba ba) -> Ba.length ba

  let cpu_exists b = b.cpu <> None
  let cpu_byte_count b = match b.cpu with 
  | None -> 0 
  | Some (Ba ba) -> Ba.length ba * (Ba.scalar_type_byte_count b.scalar_type)

  let check_ba_scalar_type b exp =
    let st = Ba.scalar_type_of_ba_scalar_type exp in 
    if st <> b.scalar_type then invalid_arg (err_st_mismatch st b.scalar_type)

  let cpu : type a b. t -> (a, b) Ba.ba_scalar_type -> (a, b) bigarray option =
    fun b st -> match b.cpu with
    | None -> None
    | Some (Ba ba) ->
        check_ba_scalar_type b st;
        (* FIXME can we do something here ? *) 
        (Obj.magic (Some ba) : (a, b) bigarray option)

  let cpu_buffer b = match b.cpu with 
  | None -> None 
  | Some (Ba ba) -> 
      let data = match b.scalar_type with 
      | `Float16 -> Some `Float 
      | `UInt32 -> Some `Unsigned 
      | `UInt64 -> Some `Unsigned 
      | _ -> None 
      in
      Some (Ba.Buffer.of_bigarray ?data ba)
      
  let get_cpu b st = match cpu b st with 
  | None -> invalid_arg err_no_cpu_buffer
  | Some cpu -> cpu 

  let get_cpu_buffer b = match cpu_buffer b with 
  | None -> invalid_arg err_no_cpu_buffer 
  | Some cpu -> cpu

  let check_kind b k =
    let open Bigarray in
    let st = b.scalar_type in
    let pass = match Obj.magic k (* FIXME *) with 
    | k when k = int8_signed -> st = `Int8
    | k when k = int16_signed -> st = `Int16
    | k when k = int32 -> st = `Int32 || st = `UInt32
    | k when k = int64 -> st = `Int64 || st = `UInt64
    | k when k = int8_unsigned -> st = `UInt8
    | k when k = int16_unsigned -> st = `UInt16 || st = `Float16
    | k when k = float32 -> st = `Float32
    | k when k = float64 -> st = `Float64
    | _ -> false
    in
    if not pass then invalid_arg (err_ba_kind st)

  let set_cpu b = function 
  | None -> b.cpu <- None
  | Some ba -> check_kind b (Bigarray.Array1.kind ba); b.cpu <- Some (Ba ba)

  let set_cpu_buffer b = function 
  | None -> b.cpu <- None 
  | Some buf ->
      match buf with 
      | `Int8 ba -> b.cpu <- Some (Ba ba)
      | `Int16 ba -> b.cpu <- Some (Ba ba)
      | `Int32 ba -> b.cpu <- Some (Ba ba) 
      | `Int64 ba -> b.cpu <- Some (Ba ba) 
      | `UInt8 ba -> b.cpu <- Some (Ba ba) 
      | `UInt16 ba -> b.cpu <- Some (Ba ba) 
      | `UInt32 ba -> b.cpu <- Some (Ba ba) 
      | `UInt64 ba -> b.cpu <- Some (Ba ba) 
      | `Float16 ba -> b.cpu <- Some (Ba ba) 
      | `Float32 ba -> b.cpu <- Some (Ba ba) 
      | `Float64 ba -> b.cpu <- Some (Ba ba)

  let cpu_p b = b.cpu
  let set_cpu_p b cpu = b.cpu <- Some cpu 
  let cpu_autorelease b = b.cpu_autorelease
  let set_cpu_autorelease b bool = b.cpu_autorelease <- bool 
  let sync_cpu_to_gpu (R ((module R), r)) buf = R.BBuf.sync_cpu_to_gpu r buf

  let pp ppf b =
    let gpu = if b.gpu_exists then (str "%d" b.gpu_count) else "none" in 
    let cpu = if b.cpu <> None then (str "%d" (cpu_count b)) else "none" in
    pp ppf "@[<1>(lit-buf %a @[<1>(cpu %s)@]@ @[<1>(gpu %s)@]@ %a)@]" 
      Ba.pp_scalar_type b.scalar_type cpu gpu pp_usage b.usage 

  (* Backend info *) 

  let binfo b = b.binfo 
  let set_binfo b i = b.binfo <- i
end

(* Attributes *)

module Attr = struct
  
  include Attr_types 
  
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
    pp ppf "@[<1>(attr %s@ %d %a@ @@%d@ +%d)@]"
      a.name a.dim Ba.pp_scalar_type a.buf.Buf.scalar_type a.first a.stride

  (* Standard attributes names *) 

  let vertex = "vertex"
  let normal = "normal" 
  let color = "color"
  let tex = "tex" 
  let texn n = if n < 0 then err_neg_arg "n" n else (str "tex%d" n)
end

(* Primitives *) 

module Prim = struct 

  include Prim_types 
    
  let pp_kind ppf kind = pp ppf begin match kind with 
    | `Points -> "points"
    | `Lines -> "lines"
    | `Line_strip -> "line-strip"
    | `Line_loop -> "line-loop"
    | `Lines_adjacency -> "lines-adjacency"
    | `Line_strip_adjacency -> "line-strip-adjacency"
    | `Triangles -> "triangles"
    | `Triangle_strip -> "triangle-strip"
    | `Triangle_fan -> "triangle-fan"
    | `Triangles_adjacency -> "triangles-adjacency"
    | `Triangle_strip_adjacency -> "triangle-strip-adjacency"
    end
 
  (* Primitives *) 

  let gen_name =
    let count = ref 0 in 
    fun () -> incr count; Printf.sprintf "prim%d" !count
        
  let create ?(tr = M4.id) ?(name = gen_name ()) ?(first = 0) ?count ?index 
      kind attrs =
    begin match index with 
    | None -> if count = None then invalid_arg err_prim_underspec else ()
    | Some b -> 
        begin match Buf.scalar_type b with 
        | `UInt8 | `UInt16 | `UInt32 | `UInt64 -> ()
        | `Int8 | `Int16 | `Int32 | `Int64
        | `Float16 | `Float32 | `Float64 as st -> 
            invalid_arg (err_prim_not_uint (str "%a" Ba.pp_scalar_type st))
        end
    end;
    let add_attr acc a = 
      let name = a.Attr.name in
      if Smap.mem name acc then invalid_arg (err_prim_attr_dup name) else 
      Smap.add name a acc
    in
    { tr; name; first; count; index; kind;
      attrs = List.fold_left add_attr Smap.empty attrs;
      binfo = BInfo.none }

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
    pp ppf "@[<1>(prim %s %a%a%a@ %dvs%a%a)@]"
      p.name pp_kind p.kind pp_tr p.tr pp_first p.first (count_now p) 
      pp_index p.index pp_attrs (attrs p)

  (* Backend info *) 

  let binfo b = b.binfo 
  let set_binfo b i = b.binfo <- i
end

(* Textures *) 

module Tex = struct
  
  include Tex_types 

  let pp_wrap ppf w = pp ppf begin match w with 
    | `Repeat -> "repeat" | `Mirrored_repeat -> "mirrored-repeat" 
    | `Clamp_to_edge -> "clamp-to-edge" 
    end

  let pp_min_filter ppf m = pp ppf begin match m with 
    | `Linear -> "linear" | `Linear_mipmap_linear -> "linear-mipmap-linear"
    | `Linear_mipmap_nearest -> "linear-mipmap_nearest" 
    | `Nearest -> "nearest" 
    | `Nearest_mipmap_linear -> "nearest-mipmap-linear"
    | `Nearest_mipmap_nearest -> "nearest-mipmap-nearest"
    end
      
  let pp_mag_filter = pp_min_filter

  let pp_kind ppf k = pp ppf begin match k with 
    | `D1 -> "D1" | `D2 -> "D2" | `D3 -> "D3" | `D2_ms -> "D2_ms" 
    | `D3_ms -> "D3_ms" | `Buffer -> "Buffer"
    end

  let pp_norm ppf b = pp ppf (if b then "normalized" else "integral")
  let pp_sample_format ppf (sf : sample_format) = match sf with
  | `D1 (st, n) -> 
      pp ppf "@[<1>(tex-sf D1 %a@ %a)@]" Ba.pp_scalar_type st pp_norm n 
  | `D2 (st, n) -> 
      pp ppf "@[<1>(tex-sf D2 %a@ %a)@]" Ba.pp_scalar_type st pp_norm n 
  | `D3 (st, n) -> 
      pp ppf "@[<1>(tex-sf D3 %a@ %a)@]" Ba.pp_scalar_type st pp_norm n 
  | `D4 (st, n) -> 
      pp ppf "@[<1>(tex-sf D4 %a@ %a)@]" Ba.pp_scalar_type st pp_norm n 
  | `SRGB st -> 
      pp ppf "@[(tex-sf sRGB %a)@]" Ba.pp_scalar_type (st :> Ba.scalar_type)
  | `SRGBA st -> 
      pp ppf "@[(tex-sf sRGBA %a)@]" Ba.pp_scalar_type (st :> Ba.scalar_type)
  | `Stencil st -> 
      pp ppf "@[<1>(tex-sf stencil %a)@]" 
        Ba.pp_scalar_type (st :> Ba.scalar_type)
  | `Depth st -> 
      pp ppf "@[(tex-sf depth %s)@]" 
        begin match st with 
        | `UInt16 -> "uint16" | `UInt24 -> "uint24" | `Float32 -> "float32" 
        end
  | `Depth_stencil st -> 
      pp ppf "@[(tex-sf depth_stencil %s)@]" 
        begin match st with 
        | `UInt24_UInt8 -> "uint24 uint8"
        | `Float32_UInt8 -> "float32 uint8"
        end

  let pp_buf_opt ppf = function 
  | None -> pp ppf "nobuf" 
  | Some b -> pp ppf "%a" Buf.pp b 

  let pp_init ppf = function
  | `D1 (sf, w, buf) -> 
      pp ppf "@[<1>(tex-init D1@ %a@ %g@ %a)@]" 
        pp_sample_format sf w pp_buf_opt buf
  | `D2 (sf, s, buf) -> 
      pp ppf "@[<1>(tex-init D2@ %a@ %a@ %a)@]" 
        pp_sample_format sf V2.pp s pp_buf_opt buf
  | `D3 (sf, s, buf) -> 
      pp ppf "@[<1>(tex-init D3@ %a@ %a %a)@]" 
        pp_sample_format sf V3.pp s pp_buf_opt buf
  | `D2_ms (sf, s, scount, fixed) -> 
      pp ppf "@[<1>(tex-init D2_ms@ %a@ %a@ %d@ %b)@]" 
        pp_sample_format sf V2.pp s scount fixed
  | `D3_ms (sf, s, scount, fixed) -> 
      pp ppf "@[<1>(tex-init D3@ %a@ %a@ %d@ %b)@]" 
        pp_sample_format sf V3.pp s scount fixed
  | `Buffer (sf, buf) ->
      pp ppf "@[<1>(tex-init Buffer %a %a)@]" 
        pp_sample_format sf Buf.pp buf 

  let init_sample_format_of_raster r norm = function
  | Some sf -> sf
  | None ->
      let rsf = Raster.sample_format r in
      let st = Raster.Sample.scalar_type rsf in
      let dim = match Raster.Sample.pack rsf with 
      | None -> Raster.Sample.dim rsf
      | Some _ -> 1 
      in
      begin match dim with 
      | 1 -> `D1 (st, norm) | 2 -> `D2 (st, norm) 
      | 3 -> `D3 (st, norm) | 4 -> `D4 (st, norm) 
      | d -> invalid_arg (err_raster_sf_dim d)
      end

  let init_of_raster ?(buf = true) ?cpu_autorelease ?usage ?kind ?sample_format
      ?(norm = true) r =
    let sample_format = init_sample_format_of_raster r norm sample_format in
    let kind = match kind with Some k -> k | None -> (Raster.kind r :> kind) in
    let buf = 
      if not buf || kind = `Buffer then None else
      Some (Buf.create ?cpu_autorelease ?usage (Raster.buffer r))
    in
    match kind with 
    | `D1 -> `D1 (sample_format, (Raster.size1 r), buf)
    | `D2 -> `D2 (sample_format, (Raster.size2 r), buf)
    | `D3 -> `D3 (sample_format, (Raster.size3 r), buf)
    | `Buffer -> 
        let buf = match buf with Some buf -> buf | None -> assert false in
        `Buffer (sample_format, buf)
    | `D2_ms | `D3_ms -> invalid_arg err_mstex
    
  let nil = 
    { kind = `D1; 
      sample_format = `D1 (`UInt8, true); 
      size = Size3.zero;
      buf = None; 
      buf_autorelease = true; 
      gpu_update = false;
      wrap_s = `Repeat; 
      wrap_t = `Repeat; 
      wrap_r = `Repeat; 
      mipmaps = false;
      multisample = None; 
      min_filter = `Nearest_mipmap_linear; 
      mag_filter = `Nearest;
      binfo = BInfo.none; }

  let create ?(wrap_s = `Repeat) ?(wrap_t = `Repeat) ?(wrap_r = `Repeat) 
      ?(mipmaps = false) ?(min_filter = `Nearest_mipmap_linear) 
      ?(mag_filter = `Nearest) ?buf_autorelease init = 
    (* TODO buffer length checks *) 
    let sformat, kind, size, buf, default_buf_autorelease, multisample = 
      match init with 
      | `D1 (fmt, s, b) -> 
          fmt, `D1, Size3.v s 1. 1., b, true, None
      | `D2 (fmt, s, b) -> 
          fmt, `D2, Size3.v (Size2.w s) (Size2.h s) 1., b, true, None
      | `D3 (fmt, s, b) -> 
          fmt, `D3, s, b, true, None
      | `D2_ms (fmt, s, scount, fixed) -> 
          fmt, `D2_ms, Size3.v (Size2.w s) (Size2.h s) 1., None, false, 
          Some (scount, fixed)
      | `D3_ms (fmt, s, scount, fixed) -> 
          fmt, `D3_ms, s, None, false, Some (scount, fixed) 
      | `Buffer (fmt, b) -> 
          fmt, `Buffer, Size3.zero, Some b, false, None
    in
    let buf_autorelease = match buf_autorelease with 
    | None -> default_buf_autorelease
    | Some b -> b
    in
    { kind; sample_format = sformat; size; 
      buf; buf_autorelease;
      gpu_update = true;
      wrap_s; wrap_t; wrap_r;
      mipmaps; min_filter; mag_filter;
      multisample;
      binfo = BInfo.none; }
  
  let sample_format t = t.sample_format
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
  let multisample t = match t.multisample with 
  | None -> invalid_arg (err_not_mstex (str "%a" pp_kind t.kind)) 
  | Some m -> m

  let pp ppf t = 
    pp ppf "@[<1>(tex@ %a@ %a@ %a@ @[<1>(wrap@ %a@ %a@ %a)@]@ \
            @[<1>(mipmaps@ %a)@]@ @[<1>(min@ %a)@]@ @[<1>(mag@ %a)@]@ %a)@]"
      pp_kind t.kind pp_sample_format t.sample_format V3.pp t.size 
      pp_wrap t.wrap_s pp_wrap t.wrap_t pp_wrap t.wrap_t
      Format.pp_print_bool t.mipmaps pp_min_filter t.min_filter 
      pp_mag_filter t.mag_filter pp_buf_opt t.buf

  (* Backend info *) 

  let binfo b = b.binfo 
  let set_binfo b i = b.binfo <- i
end

(* Uniforms *) 

module Uniform = struct

  include Uniform_types 

  (* Uniform values *) 

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
  
  include Prog_types 

  (* Source locations. *) 

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

  let insert ?loc src = 
    let stack = Printexc.get_callstack 2 in
    let loc = match loc with 
    | None -> parse_loc (Printexc.raw_backtrace_to_string stack)
    | Some loc -> loc 
    in
    loc, src

  (* Shaders *) 

  let pp_shader_stage ppf sk = pp ppf begin match sk with 
    | `Vertex -> "vertex"
    | `Tess_control -> "tess_control"
    | `Tess_evaluation -> "tess_evaluation"
    | `Geometry -> "geometry"
    | `Fragment -> "fragment"
    | `Compute -> "compute"
    end

  let shader ?lang ?loc ?(inserts = []) stage src =
    let stack = Printexc.get_callstack 2 in
    let loc = match loc with
    | None -> parse_loc (Printexc.raw_backtrace_to_string stack)
    | Some loc -> loc 
    in
    { stage; lang; srcs = (loc, src) :: (List.rev inserts) }

  let stage s = s.stage
  let loc s = fst (List.hd s.srcs) 
  let lang s = s.lang               
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
      
  let create ?(name = gen_name ()) ?(uset = Uniform.empty) shaders = 
    { name; shaders; uset; binfo = BInfo.none }

  let name p = p.name
  let uniforms p = p.uset
  let shaders p = p.shaders

  (* Backend info *) 

  let binfo e = e.binfo 
  let set_binfo e i = e.binfo <- i
end

module View = struct

  include View_types 

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
      ~bot:(-.half_h) ~top:(half_h)
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

  let default = { tr = M4.id; proj = persp (`H Float.pi_div_4) 1.5 1. 100.; 
                  viewport = Box2.unit }
                
  let create ?src ?tr ?proj ?viewport () =
    let default = match src with None -> default | Some src -> src in
    let tr = match tr with None -> default.tr | Some tr -> tr in 
    let proj = match proj with None -> default.proj | Some proj -> proj in 
    let viewport = match viewport with 
    | None -> default.viewport 
    | Some viewport -> viewport 
    in
    { tr; proj; viewport }
    
  let tr v = v.tr
  let proj v = v.proj
  let viewport v = v.viewport

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
  
  include Effect_types

  (* Rasterization state *) 
 
  let raster_default = 
    { raster_face_cull = None; 
      raster_multisample = true; } 

  (* Depth state *) 

  let depth_default = 
    { depth_test = Some `Less; 
      depth_write = true; 
      depth_offset = (0., 0.) }

  (* Blend state *) 

  let blend_eq_default = `Add (`Src_a, `One_minus_src_a)
  let blend_default = 
    { blend = false; 
      blend_rgb = blend_eq_default; 
      blend_a = blend_eq_default; 
      blend_cst = Color.void; }

  let blend_alpha = { blend_default with blend = true } 

  (* Effect *) 

  let create 
      ?(raster = raster_default) ?(depth = depth_default) 
      ?(blend = blend_default) ?uniforms prog 
    =
    let uniforms = match uniforms with 
    | None -> Prog.uniforms prog 
    | Some us -> us
    in
    { raster; depth; blend; prog; uniforms; binfo = BInfo.none }
           
  let prog e = e.prog
  let uniforms e = e.uniforms 
  let get_uniform e u = Uniform.get e.uniforms u 
  let set_uniform e u v = e.uniforms <- Uniform.def e.uniforms u v
  let raster e = e.raster
  let depth e = e.depth
  let blend e = e.blend
  
  (* Backend info *) 

  let binfo e = e.binfo
  let set_binfo e i = e.binfo <- i
end

module Fbuf = struct

  include Fbuf_types 
    
  module Rbuf = struct
    include Rbuf_types
    
    let create ?multisample size sample_format = 
      { multisample; size; sample_format; binfo = BInfo.none }

    let size2 b = b.size 
    let sample_format b = b.sample_format
    let multisample b = b.multisample
    let binfo b = b.binfo
    let set_binfo b i = b.binfo <- i
  end

  let clears_default = 
    { clear_color = Some Color.void;
      clear_depth = Some 1.; 
      clear_stencil = None; }
  
  let default = 
    { attachements = [];
      is_multisample = false;
      clears = clears_default;
      binfo = BInfo.none }

  let attachement_image = function 
  | `Color (_, i) | `Depth i | `Depth_stencil i | `Stencil i -> i

  let create ?(clears = clears_default) attachements = 
    let is_multisample = 
      let ms_attach a = match attachement_image a with
      | `Rbuf b -> Rbuf.multisample b <> None
      | `Tex (_, t) | `Tex_layer (_, _, t) -> 
          match Tex.kind t with `D2_ms | `D3_ms -> true | _ -> false 
      in
      List.exists ms_attach attachements    
    in
    { attachements; is_multisample; clears; binfo = BInfo.none }

  let attachements fb = fb.attachements
  let is_multisample fb = fb.is_multisample
  let clears fb = fb.clears
  let set_clears fb clears = fb.clears <- clears 

  let clear (R ((module R), r)) fb = R.BFbuf.clear r fb

  let pp_status ppf s = pp ppf "%s" begin match s with 
    | `Complete -> "complete"
    | `Incomplete_attachement -> "incomplete attachement" 
    | `Incomplete_draw_buffer -> "incomplete draw buffer" 
    | `Incomplete_layer_targets -> "incomplete layer targets" 
    | `Incomplete_missing_attachement -> "incomplete missing attachement"
    | `Incomplete_multisample -> "incomplete multisample"
    | `Incomplete_read_buffer -> "incomplete read buffer"
    | `Undefined -> "undefined"
    | `Unsupported -> "unsupported"
    | `Unknown c -> (str "unknown (%X)" c)
    end

  let status (R ((module R), r)) fb = R.BFbuf.status r fb        
  let read ?(first = 0) ?w_stride (R ((module R), r)) fb read box buf = 
    if is_multisample fb 
    then invalid_arg err_fb_read_msample else 
    if read = `Depth_stencil && Buf.scalar_type buf <> `UInt32 
    then invalid_arg err_fb_read_depth_stencil else
    R.BFbuf.read r fb read box ~first ~w_stride buf

  let blit ?(filter = `Nearest) (R ((module R), r)) bs ~src sbox ~dst dbox = 
    R.BFbuf.blit r filter bs ~src sbox ~dst dbox
        
  (* Backend info *) 

  let binfo f = f.binfo
  let set_binfo f i = f.binfo <- i
end

(* Renderer *) 

module Renderer = struct
  
  module Log = struct

    include Log_types 

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

    let pp_compiler_msg ppf = function 
    |  `Msg m -> pp ppf "@[%s@]" m 
    |  `Msg_loc (t, loc, m) -> pp ppf "@[%s:%a: %s@]" t Prog.pp_loc loc m
    
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
            Prog.pp_loc (Prog.loc s) Prog.pp_shader_stage (Prog.stage s) 
            (Prog.name p)
        in
        pp ppf "@[<v>%a@]" (pp_list pp_unsup) sl

    (* Logs *) 

    let of_formatter ppf level msg = pp ppf "%a@." pp_msg msg 
    let std = of_formatter Format.err_formatter
  end

  module Cap = struct

    include Cap_types 

    let shader_stages (R ((module R), r)) = (R.BCap.caps r).c_shader_stages

    (* OpenGL implementation information *) 

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
                      
    let gl_version (R ((module R), r)) = (R.BCap.caps r).c_gl_version
    let glsl_version (R ((module R), r)) = (R.BCap.caps r).c_glsl_version
    let gl_renderer (R ((module R), r)) = (R.BCap.caps r).c_gl_renderer
    let gl_vendor (R ((module R), r)) = (R.BCap.caps r).c_gl_vendor
    let pp_gl_synopsis ppf r = 
      pp ppf "@[Renderer %s@ -- OpenGL %a / GLSL %a@]" 
        (gl_renderer r) 
        pp_gl_version (gl_version r) 
        pp_gl_version (glsl_version r)

    let max_samples (R ((module R), r)) = (R.BCap.caps r).c_max_samples
    let max_tex_size (R ((module R), r)) = (R.BCap.caps r).c_max_tex_size
    let max_render_buffer_size (R ((module R), r)) = 
      (R.BCap.caps r).c_max_render_buffer_size
  end

  module Private = struct
    module BInfo = BInfo
    module Buf = Buf
    module Attr = Attr
    module Prim = Prim
    module Tex = Tex
    module Prog = Prog
    module Effect = Effect
    module Fbuf = Fbuf
    module Log = Log
    module Cap = Cap
  end

  include Renderer_types 

  type t = renderer
                             
  let op ?(count = 1) ?(uniforms = Uniform.empty) ?(tr = M4.id) effect prim = 
    { count; effect; uniforms; tr; prim }

  let nop = { count = 1; effect = Effect.create (Prog.create []); 
              uniforms = Uniform.empty; tr = M4.id; 
              prim = Prim.create ~count:0 `Triangles [] }
    
  let create ?compiler_msg_parser ?(log = Log.std) ?(debug = false) ~size 
      backend =
    let module R = (val backend : T) in
    let r = R.create ?compiler_msg_parser log ~debug size in
    R ((module R), r)
                                 
  let size (R ((module R), r)) = R.size r
  let set_size (R ((module R), r)) size = R.set_size r size
  let view (R ((module R), r)) = R.view r 
  let set_view (R ((module R), r)) v = R.set_view r v
  let fbuf (R ((module R), r)) = R.fbuf r
  let set_fbuf (R ((module R), r)) fbuf = R.set_fbuf r fbuf
  let add_op (R ((module R), r)) op = if op != nop then R.add_op r op
  let render ?(clear = true) (R ((module R), r)) = R.render r ~clear
  let release (R ((module R), r)) = R.release r
end

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
