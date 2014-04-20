(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the BSD3 license, see license at the end of the file.
   %%NAME%% release %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Lightweight OpenGL-based rendering engine. 

    Lit is a lightweight OpenGL-based rendering engine for OCaml. It
    provides a thin abstraction to program GPUs with OpenGL, OpenGL ES
    or WebGL.

    Open the module to use it, this defines only modules and types 
    in your scope.

    {b Note.} Any functions that doesn't take a renderer value 
    can be used safely even if there's no renderer setup.

    {e Release %%VERSION%% — %%MAINTAINER%% } *) 

(** {1:rtype Renderer} *) 

type renderer 
(** The type for renderers. *)

(** {1:base Buffers, primitive and textures}  *) 

open Gg


type buf 
(** The type for buffers. *) 

type attr
(** The type for vertex stream attributes. *) 

type prim
(** The type for primitives. *)

type tex 
(** The type for textures. *) 

(** Buffers.

    Buffers are linear arrays of scalars of a given {{!Gg.Ba.scalar_type}scalar
    type}. At a given point in time the scalars may exist only on the
    CPU side, only on the GPU side or on both sides (synchronized or not). *)
module Buf : sig
  
  (** {1 Buffer usage} *) 

  type usage = 
    [ `Static_draw | `Static_read | `Static_copy
    | `Stream_draw | `Stream_read | `Stream_copy
    | `Dynamic_draw | `Dynamic_read | `Dynamic_copy ]
  (** The type for buffer usage hints. First part of the name 
      identifies the data access pattern:
      {ul
      {- [`Static_*], specify once and use many times.}
      {- [`Stream_*], specify once and use once or a few times.} 
      {- [`Dynamic_*], specify and use many times.}}
      Second part of the name distinguishes the data flow: 
      {ul
      {- [`*_Draw], CPU to GPU.}
      {- [`*_Read], GPU to CPU.}
      {- [`*_Copy], GPU to GPU.}} *)

  val pp_usage : Format.formatter -> usage -> unit
  (** [pp_usage ppf usage] prints an uspecified representation of [usage] on 
      [ppf]. *) 

  (** {1 Buffers} *) 

  type ('a, 'b) init = 
    [ Gg.buffer
    | `Cpu of Ba.scalar_type * int
    | `Gpu of Ba.scalar_type * int ]
  (** The type for buffer data initialisation. 
      {ul 
      {- {!Gg.buffer} value, uses the buffer for the CPU side buffer and will 
         allocate a corresponding buffer on the GPU.}
      {- [`Cpu (st, count)], allocates a CPU side buffer of scalar type 
         [st] with [count] scalars and will allocate a corresponding buffer
         on the GPU.}
      {- [`Gpu (st, count)], allocates no CPU side buffer, will allocate 
         a GPU buffer of scalar type [st] with [count] scalars.}} *) 

  type t = buf
  (** The type for buffers. *) 
 
  val create : ?cpu_autorelease:bool ->
    ?usage:usage -> ('a, 'b) init -> buf
   (** [create cpu_autorelease usage init] is a buffer value such that:
       {ul
       {- [init] is the buffer initialisation, see {!init}.}
       {- [usage], hint specifiying the buffer usage, see {!usage} 
          (defaults to [`Static_draw]).}
       {- [cpu_autorelease], if [true] (default) the CPU buffer is
          automatically released by {{!set_cpu_}setting} it to [None] once 
          it is uploaded to the GPU buffer.}}

       Note that while CPU and GPU buffer length may change, their
       scalar type is immutable.  

       At creation time {!gpu_upload} is [true] this means that
       the GPU buffer will be automatically created the first time the
       buffer is used. *)

  val usage : buf -> usage
  (** [usage b] is the usage of [b]. *) 

  val scalar_type : buf -> Ba.scalar_type
  (** [scalar_type b] is the scalar type of [b]. *) 

  val pp : Format.formatter -> buf -> unit
  (** [pp ppf b] prints an unspecified representation of [b] on [ppf]. *)

  (** {1 GPU buffer} *) 

  val gpu_count : buf -> int
  (** [gpu_count b] is the number of scalars in the GPU buffer of [b]. *) 

  val gpu_exists : buf -> bool 
  (** [gpu_exists b] is [true] if the GPU buffer of [b] exists. *) 

  val gpu_upload : buf -> bool 
  (** [gpu_upload b] is [true] if the CPU scalars will uploaded
      to the GPU next time the renderer uses the buffer. *) 
    
  val set_gpu_upload : buf -> bool -> unit 
  (** [set_gpu_upload b u] if [u] is [true] sets [b] to upload its CPU 
      buffer to the GPU buffer next time the renderer sees the buffer. 

      {b Warning} If {!cpu} is [None] at the time of upload this will zero 
      any existing data on the GPU buffer. *) 

  val sync_gpu_to_cpu : renderer -> buf -> unit
  (** [gpu_to_cpu r b] reads back [b]'s GPU buffer into [b]'s CPU
      buffer. A CPU buffer is created in none existed yet or if the
      size of the CPU buffer is different from the size of the GPU
      buffer.

      @raise Invalid_argument if {!Buf.gpu_exists} is [false]. *)

  (** {2 Mapping the GPU buffer in CPU} *) 
    
  val gpu_map : renderer -> [ `R | `W | `RW ] -> buf -> 
    ('a, 'b) Ba.ba_scalar_type -> ('a, 'b) bigarray 
  (** [gpu_map r access b st] maps the GPU buffer of [b] with 
      access [access].
      
      {b Warning.} A mapped buffer cannot be used in a render
      operation, you need to {!unmap} it first. Once unmapped the
      bigarray becomes invalid. Don't try to access it, it may result
      in program termination.

      @raise Invalid_argument if [st] does not match [buf]'s
      scalar type. *)

  val gpu_unmap : renderer -> buf -> unit
  (** [gpu_unmap r b] unmaps the buffer [b]. 
      
      {b Warning.} This invalidates the memory pointed to 
      by the bigarray returned by {!map}, do not try to 
      access it after this function has been called it may
      result in program termination. *)

  (** {1 CPU buffer} *) 
      
  val cpu_count : buf -> int
  (** [cpu_count b] is the number of scalars in the CPU buffer of [b]. *)
    
  val cpu_exists : buf -> bool 
  (** [cpu_exists b] is [true] if the CPU buffer of [b] exists. *) 
    
  val cpu : buf -> ('a, 'b) Ba.ba_scalar_type -> ('a, 'b) bigarray option
  (** [cpu b st] is the CPU buffer of [b] (if any).
      
      {b Note.} If you want changes you made to the buffer to be picked 
      up by the GPU side buffer you must call {!set_gpu_upload}.
      
      @raise Invalid_argument if scalar type [st] is not the 
      scalar type of [b]. *)
      
  val get_cpu : buf -> ('a, 'b) Ba.ba_scalar_type -> ('a, 'b) bigarray 
  (** [get_cpu b st] is [cpu b st] but raises if there's no 
      cpu buffer.
      
      @raise Invalid_argument if [cpu b st] raises or if 
      [cpu_exists b] is [false]. *) 
      
  val set_cpu : buf -> ('a, 'b) bigarray option -> unit
  (** [set_cpu b ba] sets the CPU buffer of [b] to [ba]. 
      
      {b Note.} If you want changes to the scalars to be picked 
      up by the GPU side buffer you must call {!set_gpu_upload}.
      
      @raise Invalid_argument if the bigarray kind of [b] is not compatible
      with the scalar type of [b]. *) 
    
  val cpu_autorelease : buf -> bool 
  (** [cpu_autorelease buf] is [true] if the CPU buffer is set to [None]
      once uploaded to the GPU. *)
    
  val set_cpu_autorelease : buf -> bool -> unit
  (** [set_cpu_autorelease b bool] sets the autorelease behaviour to [bool]. *)
    
  val sync_cpu_to_gpu : renderer -> buf -> unit 
  (** [sync_cpu_to_gpu r b] uploads the CPU buffer of [b] to the GPU buffer. 
      A GPU buffer is created if none existed yet. 
      
      @raise Invalid_argument if {!Buf.cpu_exists} is [false]. *)  
end

(** Attributes. 

    An attribute is part of the vertex stream sent to the GPU by a
    {{!Prim}primitive}. It define the value of an input variable of
    the vertex shader (e.g. vertice, normal, color, etc.).

    An attribute value adds immutable metadata to a (mutable) {{!Buf}buffer}
    value in order specify the location and nature of the attribute
    elements in the buffer. *) 
module Attr : sig

  (** {1:attr Attributes} *) 

  type t = attr
  (** The type for attributes. *) 

  val create : ?normalize:bool -> ?stride:int -> ?first:int -> string ->
    dim:int -> Buf.t -> t
  (** [create normalize stride first name dim buf] is an attribute such that: 
      {ul
      {- [normalize], if [true], scalars stored as integer and 
         signed integer are respectively mapped to floating point 
         ranges [\[0;1\]] and [\[-1;1\]] on access on the GPU;
         defaults to [false].} 
      {- [stride] is the number of {e scalars} from attribute 
         element to attribute element, defaults to [dim].}
      {- [first] is the {e scalar} index of the first attribute 
         element in [buf]; defaults to 0.}
      {- [name] is the name of the attribute. This name will be used
         to bind the attribute to the corresponding input variable in 
         the vertex shader}
      {- [dim] is the dimension of the data (from [1] to [4]).}
      {- [buf] is the buffer holding the attribute elements.}} 

      @raise Invalid_argument if [dim] is out of range. *) 

  val name : attr -> string 
  (** [name a] is [a]'s name. *) 

  val dim : attr -> int 
  (** [dim a] is [a]'s dimension. *) 

  val buf : attr -> Buf.t 
  (** [buf a] is [a]'s buffer. *) 

  val stride : attr -> int 
  (** [stride a] is [a]'s attribute element scalar stride. See {!create}. *) 
 
  val first : attr -> int
  (** [first a] is [a]'s first attribute element scalar index. See {!create}. *)

  val normalize : attr -> bool
  (** [normalize a] is [a]'s normalization behaviour. See {!create}. *) 

  val rename : attr -> string -> attr 
  (** [rename a name] is like [a] except the name is changed to [name]. *)

  val pp : Format.formatter -> attr -> unit 
  (** [pp ppf a] prints an unspecified representation of [a] on [ppf]. *) 

  (** {1:std Standard attributes names} *) 

  val vertex : string
  (** [vertex] is ["vertex"]. *) 

  val normal : string 
  (** [normal] is ["normal"]. *) 

  val color : string 
  (** [color] is ["color"]. *)

  val tex : string 
  (** [tex] is ["tex"]. *)

  val texn : int -> string 
  (** [texn n] is [(Printf.sprintf "tex%d" n)]
      
      @raise Invalid_argument if [n] is negative. *)
end

(** Primitives.

    A primitive defines a vertex stream (set of {{!Attr}attributes}) sent to
    the GPU and how this stream must be interpreted as geometrical
    primitives. A primitive defines the {!kind} of primitive for 
    the geometry shader (if any) and its set of attributes define the 
    values of the input variables of the vertex shader.

    A primitive value gathers a set of attributes, an optional index buffer
    to specify the stream of vertices and a primitive {!kind}. Primitives
    are immutable but the underlying buffers are not. *)
module Prim : sig

  (** {1 Primitive kinds} *) 

  type kind = 
    [ `Points 
    | `Lines | `Line_strip | `Line_loop | `Lines_adjacency 
    | `Line_strip_adjacency 
    | `Triangles | `Triangle_strip | `Triangle_fan
    | `Triangles_adjacency | `Triangle_strip_adjacency ]
  (** The kind of primitive. Defines how the vertex stream is
      interpreted. *)
  
  val pp_kind : Format.formatter -> kind -> unit 
  (** [pp_kind ppf kind] prints an unspecified representation of [kind]
      on [ppf]. *) 

  (** {1 Primitives} *) 

  type t = prim
  (** The type for primitives. *) 

  val create : ?tr:M4.t -> ?name:string -> ?first:int -> ?count:int -> 
    ?index:Buf.t -> kind -> attr list -> prim
  (** [create name tr first count index kind attrs] is a primitive 
      such that:
      {ul
      {- [tr] is a is a transform that is right-multiplied to the render 
         operation matrix (e.g. for normalizing disparate model
         coordinate scales); TODO defaults to [id].}
      {- [name] is an optional client name for identifiying the primitive; 
         auto generated by default.}
      {- [first] is the scalar index of the first vertex index in [index], 
         ignored if [index] is unspecified; defaults to [0].}
      {- [count] is the number of {e vertices} that are part of the vertex 
         stream. [count] must be specified if [index] is unspecified. In that
         case [count] attribute elements in each attribute are read to define 
         the vertex stream.}
      {- [index] if specified must be a buffer of unsigned integers 
         used to index the attribute elements to specify the vertex stream. 
         If [count] is specified then always [count] indices will be read 
         from [index] starting at [first]. If [count] is unspecified then 
         all indices starting at [first] until the end of buffer will be read
         (hence the primitive's size may change dynamically).}
      {- [kind] the kind of primitive to render.} 
      {- [attrs], the primitive's attributes.}}
      
      @raise Invalid_argument if: 
      {ul 
      {- both [count] and [index] are unspecified}
      {- if [index] scalar type is not an unsigned integer.}
      {- if there there are two attributes of the same name in [attrs]}} *)

  val kind : prim -> kind
  (** [kind p] is the kind of [p]. *) 

  val name : prim -> string
  (** [name p] is the name of [p]. *) 

  val index : prim -> Buf.t option
  (** [index p] is the index of [p] (if any). See {!create}. *) 

  val first : prim -> int
  (** [first p] is first scalar index read from [index]. See {!create}. *) 

  val count : prim -> int option 
  (** [cout p] is the [count] of [p] as specified at primitive creation. 
      See also {!count_now}. *)
 
  val count_now : prim -> int
  (** [count_now p] is the number of vertices in the vertex stream. 
      If [count] was specified at creation time, this is always that
      number. Otherwise, let [b] be {!Prim.index}[ p] and [first] 
      be {!Prim.first}[ p]:
      {ul
      {- If {!Buf.gpu_upload}[ b] is [true], {!Buf.cpu_count}[ b - first] 
         is returned.}
      {- Otherwise {!Buf.gpu_count}[ b - first] is returned.}} *)

  val tr : prim -> M4.t
  (** [tr p] is pre-transform matrix of [p]. *) 

  val pp : Format.formatter -> prim -> unit 
  (** [pp ppf p] prints an unspecified representation of [p] on [ppf]. *) 
  
  (** {1 Primitive Attributes} *)

  val attrs : prim -> attr list 
  (** [attrs p] is the attributes of [p]. *) 

  val iter : (attr -> unit) -> prim -> unit
  (** [iter p f] iterates [f] over the attributes of [p]. *) 

  val fold : ('a -> attr -> 'a) -> 'a -> prim -> 'a 
  (** [fold f acc p] folds [f] over the attributes of [p] starting with 
      [acc]. *)

  val mem : prim -> string -> bool 
  (** [mem p n] is [true] there is an attribute with name [n] in [p]. *)

  val find : prim -> string -> attr option
  (** [find p n] is the attribute named [n] of [p] (if any). *)

  val get : prim -> string -> attr
  (** [get p n] is the attribute named [n] of [p]. 
      @raise Invalid_argument if there is no attribute named [n] in p. *)
end

(** Textures. 

    {b TODO} We need to introduce Tex.sampler which will map to GL's
    3.3 sampler objects. A tex uniform will then be a pair Tex.t *
    Tex.sampler (mmh except for Buffer textures...).  The fallback
    should simply set the parameters and we should say that in
    renderers that don't support rendering a tex with different
    Tex.sampler won't work. *)
module Tex : sig

  (** {1 Textures} *) 

  type wrap = [ `Repeat | `Mirrored_repeat | `Clamp_to_edge ]
  (** The type for texture wraps *) 

  val pp_wrap : Format.formatter -> wrap -> unit 
  (** [pp_wrap ppf w] prints a textual representation of [w] on [ppf] *)

  type mag_filter = [ `Linear | `Nearest ]
  (** The type for magnification filters. *)

  val pp_mag_filter : Format.formatter -> mag_filter -> unit 
  (** [pp_mag_filter ppf m] prints a textual representation of [m] on [ppf] *)

  type min_filter = 
    [ `Linear | `Linear_mipmap_linear | `Linear_mipmap_nearest
    | `Nearest | `Nearest_mipmap_linear | `Nearest_mipmap_nearest ]
  (** The type for minification filters. *)

  val pp_min_filter : Format.formatter -> min_filter -> unit 
  (** [pp_mag_filter ppf m] prints a textual representation of [m] on [ppf] *)

  type kind = [ `D1 | `D2 | `D3 | `D2_ms | `D3_ms | `Buffer ]
  (** The type for kinds of textures. TODO add `Cube_map *) 

  val pp_kind : Format.formatter -> kind -> unit 
  (** [pp_kind ppf k] prints a textual representation of [k] on [ppf] *)

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
  (** The type for texture sample formats. This defines 
      the internal texture format and how {!init} buffers are 
      read (dimension). For [`D1] to [`D4] the boolean indicates 
      if normalization should be performed for signed and unsigned integers
      scalar types.

      {b Note.} Renderers may not support all sample formats.
      This can by testing membership in {!Renderer.Tex.sample_formats}. *)

  val pp_sample_format : Format.formatter -> sample_format -> unit 
  (** [pp_sample_format ppf sf] prints a textual representation of [sp]
      on [ppf]. *)

  type init =
    [ `D1 of sample_format * float * Buf.t option
    | `D2 of sample_format * size2 * Buf.t option
    | `D3 of sample_format * size3 * Buf.t option
    | `D2_ms of sample_format * size2 * int * bool 
    | `D3_ms of sample_format * size3 * int * bool
    | `Buffer of sample_format * Buf.t ]
  (** The type for texture initialisation, determines the kind 
      of the the texture.

      Buffers image data pixel by pixel in row order then 
      layer order, the first pixel of the buffer is the image's 
      lower left frontmost pixel. *) 

  val pp_init : Format.formatter -> init -> unit 
  (** [pp_init ppf init] prints a textual representation of [init] on [ppf]. *)
               
  val init_of_raster : ?buf:bool -> ?cpu_autorelease:bool -> 
    ?usage:Buf.usage -> ?kind:kind -> ?sample_format:sample_format -> 
    ?norm:bool -> raster -> init
  (** [init_of_raster r] is a texture init value derived from
      [raster].  
      {ul 
      {- [buf], if [true] (default) or [kind] is [`Buffer], a Lit
         buffer value is created for the raster's buffer
         value. [cpu_autorelease] and [usage] are passed to
         {!Buf.create}.}
      {- [sample_format], if unspecified it has [r]'s sample format
         scalar type and dimension.  Packed raster sample formats are mapped
         to [`D1] texture sample format. [norm] determines the
         normalization if [sample_format] is unspecified (see
         {!type:sample_format}, defaults to [true]).}
      {- [kind], if unspecified it is automatically derived to 
         [`D1], [`D2] or [`D3] according to the raster's dimension.}} 

      @raise Invalid_argument If the raster's sample format dimension is 
      greater than 4 or if [kind] specifies a multisample texture. *)
      
  type t = tex
  (** The type for textures. *) 

  val nil : tex
  (** [nil] is a stub texture that can be used for example to initialize 
      texture uniforms. Trying to render the nil texture results in an error. *)

  val create : ?wrap_s:wrap -> ?wrap_t:wrap -> ?wrap_r:wrap -> 
    ?mipmaps:bool -> ?min_filter:min_filter -> ?mag_filter:mag_filter -> 
    ?buf_autorelease:bool -> init -> tex
    (** {ul
        {- [wrap_s, wrap_t, wrap_r], wrapping behaviour, if applicable, 
           along [s], [t] and [r] dimensions. Defaults to `Repeat.}
        {- [mipmaps], if [true] generates mipmaps. Defaults to [false].}
        {- [min_filter], if applicable, minification filter. Defaults to 
           [`Nearest_mipmap_linear].}
        {- [mag_filter], if applicable, magnification filter. Defaults to 
           [`Nearest].} 
        {- [buf_autorelease], doesn't keep a reference on buf once
           texture has been uploaded to gpu. Defaults to [true] for 
           `D1 to `D3 but false on `Buffer.}} 

        @raise Invalid_argumnet if the [init] spec is inconsistent. *) 

  val sample_format : tex -> sample_format 
  (** [sample_format t] is [t]'s sample format. *) 

  val kind : tex -> kind 
  (** [kind t] is [t]'s texture kind. *) 

  val size2 : tex -> size2 
  (** [size2 t] is [t]'s width and height. *) 

  val size3 : tex -> size3 
  (** [size3 t] is [t]'s width and height in samples. *) 

  val buf : tex -> Buf.t option
  (** [buf] is [t]'s buffer if any. *) 

  val set_buf : tex -> Buf.t option -> unit
  (** [set_buf t b] sets [t]'s buffer to b. *) 

  val buf_autorelease : tex -> bool 
  val set_buf_autorelease : tex -> bool -> unit
  val gpu_update : tex -> bool 
  val set_gpu_update : tex -> bool -> unit
  (** Needs to be called if you change the underlying buffer and 
      want buffer changes to be picked up. *)
  
  val wrap_s : tex -> wrap 
  (** [wrap_s t] is [t]'s texturing wrapping mode in the s dimension. *) 

  val wrap_t : tex -> wrap 
  (** [wrap_t t] is [t]'s texturing wrapping mode in the t dimension. *) 

  val wrap_r : tex -> wrap 
  (** [wrap_r t] is [t]'s texturing wrapping mode in the r dimension. *) 

  val mipmaps : tex -> bool
  (** [mipmaps t] is [true] if mipmaps are generated for texture. *) 

  val min_filter : tex -> min_filter 
  (** [min_filter t] is [t]'s minification filter. *) 

  val mag_filter : tex -> mag_filter 
  (** [min_filter t] is [t]'s magnification filter. *) 

  val multisample : tex -> int * bool 
  (** [multisample t] is [t]'s multisample parameters.
      
      @raise Invalid_argument if [t]'s kind is not [`D{2,3}_ms]. *) 

  val pp : Format.formatter -> tex -> unit
  (** [pp ppf t] is a textual represenation of [t] on [ppf]. *) 

end

(** {1 GPU programs and parameters} *) 


type 'a uniform 
(** The type for program uniforms. *) 

type prog 
(** The type for programs. *) 

type effect 
(** The type for effects. *) 

(** Uniforms. *) 
module Uniform : sig
  
  (** {1 Uniforms} *)

  type 'a t = 'a uniform
  (** The type for uniforms. *)

  val name : 'a t -> string 
  (** [name u] is the name of uniform [u]. *) 

  val value : 'a t -> 'a
  (** [value u] is the value of uniform [u]. 

      {b Note} Returns the zero of ['a] if [u] is a builtin. *)

  val set_value : 'a t -> 'a -> 'a t
  val v : 'a t -> 'a -> 'a t 
  (** [v] is {!set_value}. *)

  val set_to_model_to_world : m4 t -> m4 t 
  (** TODO add the rest *)

  val is_value_builtin : 'a uniform -> bool 
  (** [is_value_builtin u] is [true] if [u] has its value 
      defined by the renderer. *) 

  val pp : Format.formatter -> 'a uniform -> unit 
  (** [pp ppf u] prints an unspecified representation of [u] on [ppf]. *)

  (** {1 Uniform constructors} 

      TODO something must be said about GLSL ivec bvec etc. *) 
 
  val bool : string -> bool -> bool uniform
  (** [bool n v] is a boolean uniform named [n] with value [v]. *) 

  val int : string -> int -> int uniform 
  (** [int n v] is a integer uniform named [n] with value [v]. *) 

  val float : string -> float -> float uniform 
  (** [float n v] is a float uniform named [n] with value [v]. *) 

  val v2 : string -> v2 -> v2 uniform 
  (** [v2 n v] is a 2D vector uniform named [n] with value [v]. *) 

  val v3 : string -> v3 -> v3 uniform 
  (** [v3 n v] is a 3D vector uniform named [n] with value [v]. *) 

  val v4 : string -> v4 -> v4 uniform 
  (** [v4 n v] is a 4D vector uniform named [n] with value [v]. *) 

  val m2 : string -> m2 -> m2 uniform
  (** [m2 n v] is a 2x2 matrix uniform named [n] with value [v]. *) 

  val m3 : string -> m3 -> m3 uniform
  (** [m3 n v] is a 3x3 matrix uniform named [n] with value [v]. *) 

  val m4 : string -> m4 -> m4 uniform
  (** [m4 n v] is a 4x4 matrix uniform named [n] with value [v]. *) 

  val tex : string -> tex -> tex uniform
  (** [tex n v] is a sampler uniform named [n] with value [v]. *) 

  (** {2 Constructors for special uniform values}
      
      Those uniforms have their value automatically setup 
      by the renderer according to TODO. *)

  val model_to_world : string -> m4 uniform 
  val model_to_view : string -> m4 uniform 
  val model_to_clip : string -> m4 uniform 
  val model_normal_to_view : string -> m3 uniform 
  val world_to_view : string -> m4 uniform 
  val world_to_clip : string -> m4 uniform 
  val view_to_clip : string -> m4 uniform
  val viewport_o : string -> v2 uniform 
  (** in surface coordinates. *) 
  val viewport_size : string -> v2 uniform 
  (** in surface coordinates. *) 

  (** {1 Uniform values} *) 

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
  (** The type for built-in uniform values. *) 

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
   (** The type for untyped uniform values. *) 

  (** {1 Uniform sets} *)

  type set
  (** The type for uniform sets. *) 
    
  val empty : set
  (** [empty] is the empty set of uniforms. *) 

  val is_empty : set -> bool 
  (** [is_empty s] is [true] if [s] is empty. *) 
  
  val add : set -> 'a uniform -> set 
  (** [add s u] is [s] with [u] added. *) 

  val ( + ) : set -> 'a uniform -> set 

  val mem_named : set -> string -> bool 
  (** [mem_named s n] is true if [s] has a uniform named [n]. *)

  val find : set -> 'a uniform -> value_untyped option 
  (** [find s u] is the value of [u] in [s] (if any). *) 

  val find_named : set -> string -> value_untyped option
  (** [find s n] is the value of a uniform named [n] (if any). *) 

  val get : set -> 'a uniform -> value_untyped
  (** [find s u] is the value of [u] in [s]. 

      @raise Invalid_argument if [u] is not in [s]. *) 

  val get_named : set -> string -> value_untyped
  (** [find s u] is the value of [u] in [s]. 

      @raise Invalid_argument if [u] is not in [s]. *) 

  val fold : ('a -> string -> value_untyped -> 'a) -> 'a -> set -> 'a 
  (** [fold f acc s] folds [f] over the uniforms of [s] starting with [acc]. *)

  val pp_set : Format.formatter -> set -> unit 
  (** [pp_set ppf set] prints an unspecified representation of [s] on 
      [ppf]. *)
end

(** GPU programs 

    A GPU program is a set of linked {{!shaders}shaders}. Each shader defines 
    a transformation that is applied on the rendering pipeline that
    transforms the vertex stream to fragments. Programs can be 
    shared among effects.
*)
module Prog : sig

  (** {1:locs Source locations} 

      Depending on the GPU driver a renderer {e should} be able to
      report precise source file locations on compilation errors.

      If shader sources are loaded from external sources, specify 
      their provenance with the [loc] argument of {!insert} and
      {!shader}.

      If they are written directly in the OCaml source code files,
      compile them with [-g]. For errors to be reported without
      offset miscalculations you need to open the source string on the 
      line of the call to {!insert} or {!shader}. For example:
{v Prog.shader `Vertex " <--- This is important
  ... "
v} 

      otherwise the reported lines may be off. *) 

  type loc = [ `Loc of string * int | `Unknown ] 
  (** The type for source locations. File name and one-based line
      number or unknown. *)

  val pp_loc : Format.formatter -> loc -> unit
  (** [pp_loc ppf loc] prints an unspecified representation of [loc]
      on [ppf]. *) 

  (** {1:inserts Inserts} 
      
      Inserts can be used for source level, verbatim, textual inclusion (they
      want to be called {e includes} but that's an OCaml keyword). 
      They are a very primitive form of code reuse.*) 

  type insert
  (** The type for textual source inserts. *) 

  val insert : ?loc:loc -> string -> insert
  (** [insert ~loc:(`Loc (f, l)) src] is the insert made of [src]
      located in file [f] at line [l]. If [loc] is unspecified
      the location of the function call is used provided the program
      is compiled with [-g]. *)

  (** {1:shaders Shaders} *)

  type lang = [ `GLSL of int | `GLSL_ES of int ] 
  (** The type for shading language version and dialects. Use [150] for 
      1.50. *) 

  type shader_stage = 
    [ `Vertex | `Tess_control | `Tess_evaluation | `Geometry
    | `Fragment | `Compute ]
  (** Shader stages.
      Note that not all renderers support all shaders stages, see 
      {!Renderer.Cap.shader_stages}. *) 

  val pp_shader_stage : Format.formatter -> shader_stage -> unit 
  (** [pp_shader_stage ppf stage] prints an unspecified representatino of 
      [stage] on [ppf]. *) 
  
  type shader
  (** The type for shaders. *) 

  val shader : ?lang:lang -> ?loc:loc -> ?inserts:insert list -> 
    shader_stage -> string -> shader
  (** [shader ~loc:(`Loc (f, l)) ~inserts stage src] is the shader for
      stage [stage] made by pre-concatening the inserts [inserts] to
      [src] located in file [f] at line [l]. If [loc] is unspecified
      the location of the function call is used provided the program
      is compiled with [-g]. *)

  val stage : shader -> shader_stage
  (** [stage s] is the shader stage of [s]. *) 

  val loc : shader -> loc
  (** [loc s] is the location of shader [s]. *) 

  val lang : shader -> lang option
  (** [lang s] is the shading language for shader [s]. *) 

  type source = string * (int * string) list 
  (** The type for sources. The actual source and a map from file ids
      to file names to recognize the #line directives in the
      source. *)

  val source : ?lang:lang -> shader -> source
  (** [source ?lang s] is the source of shader [s] as will
      be given to the GPU. *)

  (** {1:programs Programs} *) 

  type t = prog 
  (** The type for programs. *) 

  val create : ?name:string -> ?uset:Uniform.set -> shader list -> prog
  (** [create uset shaders] is the program made up of [shaders], a list
      in which each {!shader_kind} should appear at most once. [uset]
      define default uniform values (defaults to {!Uniform.empty}). *)

  val name : prog -> string 
  (** [name p] is the program name. *) 
  
  val uniforms : prog -> Uniform.set 
  (** [uniforms p] are the default uniforms of [p]. *) 

  val shaders : prog -> shader list
  (** [shaders p] is the shaders of [p]. *) 
end


(** Effects.

    An effect defines a configuration of the graphics pipeline or 
    a list of effects for rendering a {!Geometry.t} value. 

    {b TODO.} Pretty printers. *)
module Effect : sig

  (** {1 Rasterization state} 

      {b Note.} Faces with a counter clockwise orientation are front 
      faces. *) 

  type raster_face_cull = [ `Front | `Back ] 
  (** The type for face culling. *) 
                          
  type raster = { raster_face_cull : raster_face_cull option } 
  (** The type for raster state. *) 

  val raster_default : raster
  (** [raster_default] is [{ cull = None }]. *) 

  (** {1 Depth state} 

      {b Note.} Depth clearing and depth range are specified 
      in renderer {!Renderer.clear}. *)

  type depth_test = 
    [ `Never | `Less | `Equal | `Lequal | `Greater | `Nequal 
    | `Gequal | `Always ]

  type depth = { depth_test : depth_test option; 
                 (** [Some _] if z-test should be performed. *)
                 depth_write : bool; 
                 (** [true] if z-buffer should be written. *)
                 depth_offset : float * float; 
                 (** factor, units, see glPolygonOffset *) }
  (** The type for depth state. *) 

  val depth_default : depth
  (** [depth] is the default depth state:
      {ul 
      {- [depth_default.depth_test] is [Some `Less].}
      {- [depth_default.depth_write] is [true].}
      {- [depth_default.depth_offset] is [(0., 0.)]}} *)
  
  (** {1 Blend state}
      
      {b TODO.}
      {ul 
      {- Support for per render buffer blend state (GL 4.0).}} *)

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
  (** The type for blend multipliers. *) 

  type blend_eq = 
    [ `Add of blend_mul * blend_mul 
    | `Sub of blend_mul * blend_mul 
    | `Rev_sub of blend_mul * blend_mul
    | `Min 
    | `Max ]
  (** The type for blend equations.
      {ul
      {- [`Add (a, b)] is [a * src + b * dst].}
      {- [`Sub (a, b)] is [a * src - b * dst].}
      {- [`Rev_sub (a, b)] is [b * dst - a * src].}
      {- [`Min], component wise [min a b].}
      {- [`Max], component wise [max a b].}} *)

  val blend_eq_default : blend_eq
  (** [blend_default_eq] is [`Add (Src_a, One_minus_src_a)]. *) 

  type blend =  
    { blend : bool; (** [true] if blending should be performed. *)
      blend_rgb : blend_eq; (** Blending equation for rgb *) 
      blend_a : blend_eq; (** Blending equation for alpha. *)
      blend_cst : color; (** Blend color for [`Cst_*] multiplier. *)  }
  (** The type for blend states. *)

  val blend_default : blend
  (** [blend_default] is the default blend state:
      {ul
      {- [blend_default.blend] is [false].}
      {- [blend_default.blend_rgb] is {!blend_default_eq}.}
      {- [blend_default.blend_a] is {!blend_default_eq}.}
      {- [blend_default.blenc_cst] is {!Gg.Color.void}.}} *)

  val blend_alpha : blend
  (** [blend_alpha] is [blend_default] with the [blend] field 
      to [true]. *) 

  (** {1 Effect} *) 
    
  type t = effect
  
  val create : ?raster:raster -> ?depth:depth -> ?blend:blend ->
    ?uniforms:Uniform.set -> 
    prog -> effect   
  (** [create raster depth blend uniforms prog] is an effect that uses the GPU 
      program [prog]. [raster], [depth] and [blend] respectively define 
      the rasterization, depth buffer and blend states (resp. default 
      to {!raster_default}, {!depth_default}, {!blend_default}). 
      [uniforms] is the uniforms used with [prog] (defaults to 
      [Prog.uniforms (prog e)]). *) 
    
  val prog : effect -> prog
  (** [prog e] is [e]'s GPU program. *) 

  val uniforms : effect -> Uniform.set 
  (** [uniforms e] the uniforms that are used with [e]'s program. *) 

  val get_uniform : effect -> 'a uniform -> Uniform.value_untyped 
  (** [get_uniform e u] is the value of uniform [u] in [e]. *) 

  val set_uniform : effect -> 'a uniform -> 'a -> unit
  (** [set_uniform e u v] sets the value of uniform [u] to [v] in [e]. *) 

  val raster : effect -> raster 
  (** [raster e] is [e]'s raster state. *) 

  val depth : effect -> depth
  (** [depth e] is [e]'s depth state. *) 

  val blend : effect -> blend 
  (** [blend e] is [e]'s blend state. *) 
end

(** {1 Rendering} *) 


type view
(** The type for rendered views. *)

type fbuf 
(** The type for framebuffers. *) 

(** Views. 

    A view defines a view volume in 3D space mapped to clipped 
    space.

    A view defines the rendered view volume in 3D space and the
    viewport on which rendering occurs on the renderer's surface.

    The view volume is specified by a view transform {!tr} that
    locates and orients the view volume and a projection transform
    {!proj} that defines the shape of the volume. Multiplied toghether
    these transform map world space to clip space.

    The view's {!viewport} defines, in normalized surface coordinates,
    a rectangular area of the surface. The normalized device coordinates
    are mapped on the viewport. *)
module View : sig

  (** {1 View} *) 

  type t = view
  (** The type for views. *)

  val create : ?tr:m4 -> ?proj:m4 -> ?viewport:box2  -> unit  -> t
  (** [create tr proj viewport] is a view such that: 
      {ul 
      {- [tr], defines the location and orientation of the view.
         It is the transform that maps world coordinates to view 
         coordinates. Defaults to {!M4.id}, i.e. we are at the origin
         looking at down the z-axis. It defines the builtin uniform 
         value {!Uniform.world_to_view}.}
      {- [proj], defines the viewing volume. It is the transform 
         that maps view coordinates to clip space. Defaults to 
         {!persp}[(`H Float.pi_div_4) 1.5 1. 100.]. It defines the 
         builtin uniform value {!Uniform.view_to_clip}.}
      {- [viewport] a rectangular area in normalized screen
         coordinates of the renderer's viewport; default is
         {!Box2.unit}. It defines the builtin uniform values 
         {!Uniform.viewport_o} and {!Uniform.viewport_size}.
      }} *)

  val tr : view -> m4 
  val set_tr : view -> m4 -> unit
  val proj : view -> m4 
  val set_proj : view -> m4 -> unit
  val viewport : view -> box2 
  val set_viewport : view -> box2 -> unit 
    

  (** {1 Coordinate system transforms} 
      
      TODO explain in detail coordinate systems *)

  val viewport_of_surface : view -> p2 -> p2 
  (** [viewport_of_surface view pt] is the normalized viewport coordinates
      in [view] of the normalized surface coordinates [pt]. *) 

  val viewport_of_ndc : view -> p2 -> p2 
  (** [viewport_of_ndc view pt] is the normalized viewport coordinates
      in [view] of the normalized device coordinates [pt] in [view]. *) 

  val surface_of_viewport : view -> p2 -> p2 
  (** [surface_of_viewport view pt] is the normalized surface coordinates
      of the normalized viewport coordinates [pt] in [view]. *) 

  val surface_of_ndc : view -> p2 -> p2 
  (** [surface_of_ndc view pt] is the normalized surface coordinates
      of the normalized device coordinates [pt] in [view]. *) 

  val ndc_of_viewport : view -> p2 -> p2 
  (** [ndc_of_viewport view pt] is the normalized device coordinates
      in [view] of the normalized viewport coordinates [pt] in [view]. *) 

  val ndc_of_surface : view -> p2 -> p2 
  (** [ndc_of_surface view pt] is the normalized device coordinates
      of the normalized surface coordinates [pt] in [view]. *) 

  (** {1 Projections and view matrices} *)
       
  type fov = [ `H of float | `V of float ] 
  (** The type for field of view angles along horizontal or vertical axes. *)

  val persp : fov:fov -> aspect:float -> near:float -> far:float -> m4
  (** [persp fov aspect near far] is a perspective projection matrix 
      such that:
      {ul 
      {- [fov] is the field of view angle in radians along a given 
         axis.}
      {- [aspect] is the ratio between the horizontal field of view 
         and the vertical field of view.}
      {- [near] and [far] are {e positive} distances to the near and 
         far clip planes.}}

      The transform is defined as follows according to [fov]. 
      {ul
      {- If [fov = `V fovy], then let [h = 2 * near * (tan (fovy / 2))] 
      and [w = aspect * h].}
      {- If [fov = `H fovx], then let [h = w / aspec] and 
      [w = 2 * near * (tan (fovx / 2))]}}
      The projection maps the symmetric frustum with top of the underlying
      pyramid at the origin, near clip rectangle corners
      [(-w/2,-h/2,-near)], [(w/2,h/2,-near)] and far plane at [-far]
      to the axis aligned cube with corners [(-1, -1, -1)] and [(1,1,1)].
  *) 

  val look : ?up:v3 -> at:p3 -> from:p3 -> unit -> m4
  (** [look up at ~from:pos ()] in layman terms this is the transform
      which has the effect of putting you at position [pos] looking at
      the point [at] and with your head tilted to match the [up]
      direction.

      More precisely, the transform maps:
      {ul 
      {- [pos] on the origin {!P3.o}}
      {- [oz' = V3.(unit (from - at))] to {!V3.oz}}
      {- [ox' = V3.(unit (cross up oz'))] to {!V3.ox}}
      {- [oy' = V3.(unit (cross oz ox'))] to {!V3.oy}}} 
      The final up vector [oy'] matches exactly up only if [up]
      is orthogonal the the forward direction [at - from]. *)
end

(** Framebuffers. *)
module Fbuf : sig

  (** Render buffers. 

      Render buffers are images that can be attached to framebuffers
      but in contrast to textures cannot be used by shaders. *) 
  module Rbuf : sig
    type t
    (** The type for render buffers. *)

    val create : ?multisample:int -> size2 -> Tex.sample_format -> t
    (** [create mutisample size fmt] is an image with 
        size [size] and sample format [fmt]. [multisample] is the number
        of sample for multisample framebuffers. *)

    val multisample : t -> int option 
    (** [multisample b] is [b]'s multisample value. *) 

    val size2 : t -> size2 
    (** [size2 b] is [b]'s size. *) 

    val sample_format : t -> Tex.sample_format 
    (** [sample_format b] is [b]'s sample format. *) 
  end

  type image = 
    [ `Tex of int * tex | `Tex_layer of int * int * tex | `Rbuf of Rbuf.t ] 
  (** The type for images. *) 
  
  type attachement = 
    [ `Color of int * image 
    | `Depth of image | `Stencil of image | `Depth_stencil of image ]
  (** The type for image attachements. *) 

  type t = fbuf 
  (** The type for framebuffers. *) 

  val default : fbuf
  (** [default] is the default framebuffer. *) 
  
  val create : attachement list -> fbuf 
  (** [create attachements] is a framebuffer with attachements [attachments]. *)

  val attachements : fbuf -> attachement list 
  (** [attachements f] is [f]'s attachements. *) 
      
  (** {1 Framebuffer status} *) 

  type status = 
    [ `Complete
    | `Incomplete_attachement
    | `Incomplete_draw_buffer
    | `Incomplete_layer_targets
    | `Incomplete_missing_attachement
    | `Incomplete_multisample
    | `Incomplete_read_buffer
    | `Undefined
    | `Unsupported ]      
    
  val status : renderer -> fbuf -> status

  (** {1 Framebuffer reading} *) 
  
  type read = 
    [ `Color_r of int 
    | `Color_g of int 
    | `Color_b of int 
    | `Color_rgb of int 
    | `Color_rgba of int
    | `Depth
    | `Stencil 
    | `Depth_stencil ]
  (** The type for framebuffer reads. For color components the
        integer denotes the color attachement (ignored for {!default}).  *) 

  val read : renderer -> fbuf -> read -> pos:p2 -> size:size2 -> 
    buf -> unit
  (** [read t f fmt buf] asynchronously reads the contents of
        framebuffer [f] according to [fmt] and stores the result in
        the GPU buffer of [buf].
      
        @raise Invalid_argument if [`Depth_stencil] is used and 
        the scalar type of [buf] is not `UInt32 (the data is packed
        as 24 bits of depth and 8 bits of stencil). *)
end

type op = 
  { count : int; effect : effect; uniforms : Uniform.set; tr : m4; prim : prim }
(** The type for render operations. *) 

(** Renderers *) 
module Renderer : sig

  (** {1 Logging and renderer capabilities} *) 

  module Log : sig

    (** {1 Shader compiler messages} *) 

    type compiler_msg = 
      [ `Msg of string | `Msg_loc of string * Prog.loc * string ]
    (** The type for shader compiler messages. *) 

    val pp_compiler_msg : Format.formatter -> compiler_msg -> unit 
    (** [pp_compiler_msg ppf m] prints an unspecified representation of [m]
        on [ppf]. *)
    
    type compiler_msg_parser = string -> 
      [ `Loc of string * int * int * string | `Unparsed of string  ]
    (** The type for shader compiler message parsers. Parses a line into
        either:
        {ul
        {- [`Loc (before, file_id, line, after)], for error messages
           with file (which are represented by numbers) and line locations.}
        {- [`Unparsed msg] if no location can be parsed in the error 
           message}} *)
                                
    val compiler_msg_parser_default : compiler_msg_parser
    (** [compiler_parser_default] parses the following patterns into [`Loc]:
        {ul
        {- msg:int:int:msg}
        {- int:int:msg}}
        Otherwise returns [`Unparsed]. *)
      
    val compiler_msg_parser_raw : compiler_msg_parser
    (** [compiler_parser_raw s] is always [`Unparsed]. This allows
        to get the raw log.  *)

    (** {1 Renderer messages} *) 

    type msg = 
      [ `Compiler of compiler_msg list
      | `Linker of string list 
      | `Missing_attr of prim * string
      | `Unsupported_shaders of prog * Prog.shader list 
      | `Msg of string ]
    (** The type for log messages. 
        {ul 
        {- [`Compiler msgs] shader compilation log.} 
        {- [`Linker msgs] GPU program linker log.} 
        {- [`Missing_attr (prim, att)] primitive [prim] is missing an attribute
           named [att]} 
        {- [`Unsupported_shaders (prog, sl)] program [prog] is using 
           shaders [sl] whose kind is unsupported by the OpenGL renderer.}
        {- [`Msg msg] generic message TODO remove that ?.}} *)

    val pp_msg : Format.formatter -> msg -> unit 
    (** [pp_msg ppf m] prints an unspecified representation of [m] on 
        [ppf]. *) 

    (** {1:logs Logs} *) 

    type level = [ `Error | `Debug ] 
    (** The type for log levels. *)
    
    type t = level -> msg -> unit
    (** The type for logs. *) 

    val of_formatter : Format.formatter -> t 
    (** [of_formatter ppf] is a log that outputs on [ppf]. *) 
  end

  (** Renderer capabilities. *)
  module Cap : sig

    (** {1 Renderer capabilities and limits} *) 

    val shader_stages : renderer -> Prog.shader_stage list
    (** [shader_stages r] is the list of {{!Prog.shader_stages}shader stages} 
        supported by the renderer. *)

    val max_samples : renderer -> int
    val max_tex_size : renderer -> int
    val max_render_buffer_size : renderer -> int

    (** {1 OpenGL implementation information} *) 
    
    type gl_version = 
      [ `GL of (int * int * int) | `GLES of (int * int * int) | `Unknown ] 
    (** The type for GL versions. *) 

    val pp_gl_version : Format.formatter -> gl_version -> unit 
    (** [pp_gl_version ppf v] prints an unspecified representation of [v]
        on [ppf]. *) 

    val gl_version : renderer -> gl_version
    (** [gl_version r] is the OpenGL version number of [r]. *) 

    val glsl_version : renderer -> gl_version 
    (** [gl_version r] is the shading language version number of [r]. *) 

    val gl_renderer : renderer -> string 
    (** [gl_renderer r] is the OpenGL renderer of [r]. *) 

    val gl_vendor : renderer -> string 
    (** [gl_vendor r] is the OpenGL renderer vendor of [r]. *) 

    val pp_gl_synopsis : Format.formatter -> renderer -> unit
    (** [pp_gl_synopsis ppf r] prints a short two lines summary 
        of the OpenGL implementation [r] is dealing with. *)
  end

  (** {1:ops Render operations} *) 

  val op : ?count:int -> ?uniforms:Uniform.set -> ?tr:m4 -> effect -> prim -> op
  (** [op count uniforms tr e p] is a render op. [count] defaults to 1. 
      [uniforms] defaults to {!Uniform.empty}, [tr] to {!M4.id}. *)
    
  val nop : op 
  (** [nop] is a render no-op, it has no effect on the renderer. *) 

  val add_op : renderer -> op -> unit
  (** [add_op r o] adds render operation [o] on [r]. *) 

  (** {1 Window geometry and projection} *)

  val size : renderer -> size2
  val set_size : renderer -> size2 -> unit
  val view : renderer -> View.t
  val set_view : renderer -> View.t -> unit

  (** {1 Framebuffers} *) 
    
  val fbuf : renderer -> fbuf
  (** [fbuf r] is the framebuffer on which render ops are performed. *)
    
  val set_fbuf : renderer -> fbuf -> unit
  (** [set_fbuf r fbuf] sets the framebuffer to [fbuf]. *) 
     
  (** {1 Clearing state} *)

  type clears = 
    { clear_color : color option; 
      clear_depth : float option; 
      clear_stencil : int option; }

  val clears_default : clears  
  val clears : renderer -> clears
  val set_clears : renderer -> clears -> unit
(*  val clear : renderer -> unit  *)

  (** {1 Rendering} *) 

  val render : ?clear:bool -> renderer -> unit
  (** [render clear r] renders the added operations. If [clear] is 
      [true] (default) the buffers are cleared before rendering. *) 

  (** {1 Renderers} *) 

  (** Private functions and types for implementing renderers. 
  
      {b Warning.}  [Lit] users should not use these definitions. They 
      expose [Lit]'s internals and are subject to change even between 
      minor versions of the library. *)
  module Private : sig

    module Id : sig
      type t = int
      val compare : t -> t -> int
    end

    module BInfo : sig
      type t 
      val create : unit -> ('a -> t) * (t -> 'a option)
      val none : t
    end

    module Buf : sig
      include module type of Buf
      type bigarray_any = Ba : ('a, 'b) bigarray -> bigarray_any
      val create_bigarray_any : Gg.Ba.scalar_type -> int -> bigarray_any
      val gpu_byte_count : buf -> int 
      val set_gpu_count : buf -> int -> unit 
      val set_gpu_exists : buf -> bool -> unit
      val cpu_byte_count : buf -> int 
      val cpu_p : buf -> bigarray_any option
      val set_cpu_p : buf -> bigarray_any -> unit
      val check_ba_scalar_type : buf -> ('a, 'b) Ba.ba_scalar_type -> unit
      val binfo : buf -> BInfo.t
      val set_binfo : buf -> BInfo.t -> unit
    end
   
    module Attr : sig 
      include module type of Attr
    end

    module Prim : sig 
      include module type of Prim
      val binfo : prim -> BInfo.t
      val set_binfo : prim -> BInfo.t -> unit
    end

    module Tex : sig 
      include module type of Tex 
      val binfo : tex -> BInfo.t 
      val set_binfo : tex -> BInfo.t -> unit
    end

    module Prog : sig 
      include module type of Prog with type shader = Prog.shader
      val binfo : prog -> BInfo.t
      val set_binfo : prog -> BInfo.t -> unit
    end

    module Effect : sig
      include module type of Effect with type raster = Effect.raster
                                     and type depth = Effect.depth 
                                     and type blend = Effect.blend
                                                         
      val binfo : effect -> BInfo.t
      val set_binfo : effect -> BInfo.t -> unit
    end

    module Fbuf : sig 
      module Rbuf : sig 
        include module type of Fbuf.Rbuf with type t = Fbuf.Rbuf.t
        val binfo : t -> BInfo.t 
        val set_binfo : t -> BInfo.t -> unit
      end
      include module type of Fbuf with module Rbuf := Rbuf
      val binfo : fbuf -> BInfo.t 
      val set_binfo : fbuf -> BInfo.t -> unit
    end
    
    module Log : sig 
      include module type of Log 
      val split_string : char -> string -> string list 
      val lines : string -> string list           
      val compiler_msg : string -> compiler_msg_parser ->
        (int * string) list -> [> `Compiler of compiler_msg list ]
    end

    module Cap : sig
      val parse_version : string -> (int * int * int) option 
      type t = 
        { c_shader_stages : Prog.shader_stage list; 
          c_max_samples : int;
          c_max_tex_size : int; 
          c_max_render_buffer_size : int;
          c_gl_version : Cap.gl_version; 
          c_glsl_version : Cap.gl_version; 
          c_gl_renderer : string;
          c_gl_vendor : string; }
    end
  end

  (** The type for a renderer backend. *) 
  module type T = sig
    
    type t 

    module BCap : sig
      val caps : t -> Private.Cap.t 
    end

    module BBuf : sig 
      val sync_cpu_to_gpu : t -> buf -> unit
      val sync_gpu_to_cpu : t -> buf -> unit
      val gpu_map : t -> [ `R | `W | `RW ]  -> buf -> 
        ('a, 'b) Ba.ba_scalar_type -> ('a, 'b) bigarray 
      val gpu_unmap : t -> buf -> unit
    end

    module BFbuf : sig        
      val status : t -> fbuf -> Fbuf.status
      val read : t -> fbuf -> Fbuf.read -> pos:p2 -> size:size2 -> 
        buf -> unit
    end

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
    val fbuf : t -> fbuf 
    val set_fbuf : t -> fbuf -> unit
    val add_op : t -> op -> unit
    val render : t -> clear:bool -> unit
    val release : t -> unit
  end
  
  type t = renderer
  (** The type for renderers. *)

  val create : 
    ?compiler_msg_parser:Log.compiler_msg_parser -> 
    ?log:Log.t -> 
    ?debug:bool -> 
    size:size2 -> 
    (module T) -> 
    renderer
  (** [creates size b] creates a renderer with backend [b]. 
      A valid OpenGL context is needed before calling this function. *)

  val release : renderer -> unit
  (** [release r] releases GPU resources associated to the renderer. *) 

end
  


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
