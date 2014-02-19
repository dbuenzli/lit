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

    {e Release %%VERSION%% — %%MAINTAINER%% } *) 

(** {1:bufs Buffers}  *) 

open Gg

type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t

type buf 
(** The type for buffers. *) 

(** A few bigarray helpers. *) 
module Ba : sig

  (** {1:create Creation} *)
  
  val create : ('a, 'b) Bigarray.kind -> int -> ('a, 'b) bigarray 
  (** [create k count] is a bigarray of kind [k] with [count] scalars. *)

  (** {1:get Getting} *) 

  val get_v2 : (float, 'b) bigarray -> int -> v2 
  (** [get_v2 b i] is the [i]th to [i+1]th scalars of [b] as a vector. *) 

  val get_v3 : (float, 'b) bigarray -> int -> v3 
  (** [get_v3 b i] is the [i]th to [i+2]th scalars of [b] as a vector. *) 

  val get_v4 : (float, 'b) bigarray -> int -> v4 
  (** [get_v4 b i] is the [i]th to [i+3]th scalars of [b] as a vector. *) 

  val get_2d : ('a, 'b) bigarray -> int -> ('a * 'a)
  (** [get_v2 b i] is the [i]th to [i+1]th scalars of [b]. *)

  val get_3d : ('a, 'b) bigarray -> int -> ('a * 'a * 'a) 
  (** [get_v3 b i] is the [i]th to [i+2]th scalars of [b]. *)

  val get_4d : ('a, 'b) bigarray -> int -> ('a * 'a * 'a * 'a)
  (** [get_v4 b i] is the [i]th to [i+3]th scalars of [b]. *) 

  (** {2 int32 Bigarray} *)

  val geti_2d : (int32, 'b) bigarray -> int -> (int * int)
  (** [get_v2 b i] is the [i]th to [i+1]th scalars of [b]. *)

  val geti_3d : (int32, 'b) bigarray -> int -> (int * int * int) 
  (** [get_v3 b i] is the [i]th to [i+2]th scalars of [b]. *)
  
  (** {1:set Setting} *) 

  val set_v2 : (float, 'b) bigarray -> int -> v2 -> int
  (** [set_v2 b i v] is [(i + 2)] and sets the [i]th to [i+1]th scalars of [b] 
      with [v]. *) 

  val set_v3 : (float, 'b) bigarray -> int -> v3 -> int
  (** [set_v3 b i v] is [(i + 3)] and sets the [i]th to [i+2]th scalars of [b] 
      with [v]. *) 

  val set_v4 : (float, 'b) bigarray -> int -> v4 -> int
  (** [set_v4 b i v] is [(i + 4)] and sets the [i]th to [i+3]th scalars of [b] 
      with [v]. *) 

  val set_2d : ('a, 'b) bigarray -> int -> 'a -> 'a -> int
  (** [set_2d b i s1 s2] is [(i + 2)] and sets the [i]th to [i+1]th scalar 
      of [b] to [s1], [s2]. *) 

  val set_3d : ('a, 'b) bigarray -> int -> 'a -> 'a -> 'a -> int
  (** [set_3d b i s1 s2 s3] is [(i + 3)] and sets the [i]th to [i+2]th scalar 
      of [b] to [s1], [s2], [s3]. *) 

  val set_4d : ('a, 'b) bigarray -> int -> 'a -> 'a -> 'a -> 'a -> int
  (** [set_4d b i s1 s2 s3 s4] is [(i + 4)] and sets the [i]th to [i+3]th 
      scalar of [b] to [s1], [s2], [s3], [s4]. *)

  (** {2 int32 Bigarray} *)

  val seti_2d : (int32, 'b) bigarray -> int -> int -> int -> int
  (** [set_2d b i s1 s2] is [(i + 2)] and sets the [i]th to [i+1]th scalar 
      of [b] to [s1], [s2]. *) 

  val seti_3d : (int32, 'b) bigarray -> int -> int -> int -> int -> int
  (** [set_3d b i s1 s2 s3] is [(i + 3)] and sets the [i]th to [i+2]th scalar 
      of [b] to [s1], [s2], [s3]. *) 
end

(** Buffers.

    Buffers are linear arrays of scalars of a given {{!type:scalar_type}scalar
    type}. At a given point in time the scalars may exist only on the
    CPU side, only on the GPU side or on both sides (synchronized or not).

    Some functions on buffers need a renderer, see {!Renderer.Buf}. *)
module Buf : sig

  (** {1 Scalar types} *) 

  type scalar_type = 
    [ `UInt8 | `Int8 | `UInt16 | `Int16 | `UInt32 | `Int32 
    | `Float32 | `Float64 ] 
  (** The type for scalar types. *) 

  val scalar_type_byte_count : scalar_type -> int
  (** [scalar_type_byte_count st] is the number of bytes used by [st]. *) 

  val scalar_type_to_bigarray_kind : scalar_type -> ('a, 'b) Bigarray.kind
  (** [scalar_type_to_bigarray_kind st] is the bigarray kind of [st] *)

  val scalar_type_of_bigarray_kind : ?unsigned:bool -> 
    ('a, 'b) Bigarray.kind -> scalar_type option
  (** [scalar_type_of_bigarray_kind k] is the scalar type of [k] (if any). 
      [unsigned] is only used for indicating wheter {!Bigarray.int32} 
      should be mapped to [`UInt32] rather than [`Int32] 
      (defaults to [false]). *)

  val scalar_type_to_string : scalar_type -> string 
  (** [scalar_type_to_string st] is an unspecified string representing
      [st]. *) 

  val pp_scalar_type : Format.formatter -> scalar_type -> unit
  (** [pp_scalar_type ppf st] prints and unspecified representation 
      of [st] on [ppf]. *) 

  (** {1 Buffers} *) 

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

  type ('a, 'b) init = 
    [ `Cpu of scalar_type * int
    | `Bigarray of ('a, 'b) bigarray
    | `Gpu of scalar_type * int ]
  (** The type for buffer data initialisation. 
      {ul 
      {- [`Cpu (st, count)], allocates a CPU side buffer of scalar type 
         [st] with [count] scalars and will allocate a corresponding buffer
         on the GPU.}
      {- [`Bigarray b], uses [b] for the CPU side buffer and will 
         allocate a corresponding buffer on the GPU.}
      {- [`Gpu (st, count)], allocates no CPU side buffer, will allocate 
         a GPU buffer of scalar type [st] with [count] scalars.}} *) 

  type t = buf
  (** The type for buffers. *) 
 
  val create : ?unsigned:bool -> ?cpu_autorelease:bool ->
    ?usage:usage -> ('a, 'b) init -> buf
   (** [create unsigned cpu_autorelease usage init] is a buffer value such that:
       {ul
       {- [init] is the buffer initialisation, see {!init}. Note that 
          for [`Cpu] and [`Cpu_bigarray] initialisation, {!gpu_upload}
          is [true] on the resulting buffer.}
       {- [usage], hint specifiying the buffer usage, see {!usage} 
          (defaults to [`Static_draw]).}
       {- [cpu_autorelease], if [true] (default) the CPU buffer is
          automatically released by {{!set_cpu_}setting} it to [None] once 
          it is uploaded to the GPU buffer.}
       {- [unsigned] is used to treat a {!Bigarray.int32} in a [`Cpu_bigarray]
          initialisation as a [`UInt32] scalar type (defaults to [false]).}}

       Note that while CPU and GPU buffer length may change, their scalar 
       type is immutable.
       
       @raise Invalid_argument if the bigarray in [`Cpu_bigarray] cannot
       be converted to a {{!type:scalar_type}scalar_type}. *) 

  val usage : buf -> usage
  (** [usage b] is the usage of [b]. *) 

  val scalar_type : buf -> scalar_type
  (** [scalar_type b] is the scalar type of [b]. *) 

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

  val cpu_count : buf -> int
  (** [cpu_count b] is the number of scalars in the CPU buffer of [b]. *)

  val cpu : buf -> ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray option
  (** [cpu b kind] is the CPU buffer of [b] (if any).
      
      {b Note.} If you want changes you made to the buffer to be picked 
      up by the GPU side buffer you must call {!set_gpu_upload}.

      @raise Invalid_argument if [kind] doesn't correspond to the 
      scalar type of [b]. *)

  val set_cpu : buf -> ('a, 'b) bigarray option -> unit
  (** [set_cpu b ba] sets the CPU buffer of [b] to [ba]. 

      {b Note.} If you want changes to the scalars to be picked 
      up by the GPU side buffer you must call {!set_gpu_upload}.

      @raise Invalid_argument if the bigarray kind of [b] doesn't 
      correspond to the scalar type of [b]. *) 

  val cpu_autorelease : buf -> bool 
  (** [cpu_autorelease buf] is [true] if the CPU buffer is set to [None]
      once uploaded to the GPU. *)

  val set_cpu_autorelease : buf -> bool -> unit
  (** [set_cpu_autorelease b bool] sets the autorelease behaviour to [bool]. *)

  val pp : Format.formatter -> buf -> unit
  (** [pp ppf b] prints an unspecified representation of [b] on [ppf]. *)
end

(** {1:prims Primitives} *) 

type attr
(** The type for vertex stream attributes. *) 

type prim
(** The type for primitives. *)


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
      {- [stride] is the number of {e scalars} to skip in [buf] 
         between two consecutive attribute elements; defaults to [0].}
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
end

(** Primitives 

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

  val count : prim -> int
  (** [count p] is the number of vertices in the vertex stream. 
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

(** {1 GPU programs and parameters} *) 

type tex 
(** The type for textures. *) 

type 'a uniform 
(** The type for program uniforms. *) 

type prog 
(** The type for programs. *) 

type effect 
(** The type for effects. *) 

(** Textures. *)
module Tex : sig
  type t
end

(** Uniforms. *) 
module Uniform : sig

  (** {1 Uniform values} *) 

  (** The type for typed uniform values. *)
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
    | Model_normal_to_view : m4 value 
    | World_to_view : m4 value
    | World_to_clip : m4 value 
    | Viewport_o : v2 value
    | Viewport_size : v2 value
    | Time : float value

  type builtin = 
    [ `Model_to_world 
    | `Model_to_view 
    | `Model_to_clip 
    | `Model_normal_to_view 
    | `World_to_view 
    | `World_to_clip 
    | `Viewport_o
    | `Viewport_size 
    | `Time ]
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
  
  (** {1 Uniforms} *) 

  type 'a t = 'a uniform
  (** The type for uniforms. *)

  val u : string -> 'a value -> 'a uniform
  (** [u name v] is a uniform named [name] whose initial value is [v]. *) 

  val name : 'a t -> string 
  (** [name u] is the name of uniform [u]. *) 

  val value : 'a t -> 'a value 
  (** [value u] is the value of uniform [u]. *) 

  val pp : Format.formatter -> 'a uniform -> unit 
  (** [pp ppf u] prints an unspecified representation of [u] on [ppf]. *)

  (** {1 Uniform sets} *)

  type set
  (** The type for uniform sets. *) 
    
  val empty : set
  (** [empty] is the empty set of uniforms. *) 

  val is_empty : set -> bool 
  (** [is_empty s] is [true] if [s] is empty. *) 
  
  val add : set -> 'a uniform -> set 
  (** [add s u] is [s] with [u] added. *) 
  
  val set : set -> 'a uniform -> set * 'a uniform
  (** [set s u] is [(s', u)] where [s'] is [s] with [u] added. *) 

  val def : set -> 'a uniform -> 'a -> set
  (** [def s u v] is [s] with [u] having value [v]. *)

  val def_v : set -> 'a uniform -> 'a value -> set
  (** [def_v s u v] is [s] with [u] having value [v]. *) 

  val def_named : set -> string -> value_untyped -> set
  (** [def_named s n v] is [s] with a uniform named [n] having value [v]. *)

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

  type shader_kind = 
    [ `Vertex | `Tess_control | `Tess_evaluation | `Geometry
    | `Fragment | `Compute ]
  (** Kinds of shaders. 
      Note that not all renderers support all shaders, see 
      {!Renderer.Cap.shader_kinds}. *) 

  val pp_shader_kind : Format.formatter -> shader_kind -> unit 
  (** [pp_shader_kind ppf k] prints an unspecified representatino of [k]
      on [ppf]. *) 
  
  type shader
  (** The type for shaders. *) 

  val shader : ?lang:lang -> ?loc:loc -> ?inserts:insert list -> shader_kind -> 
    string -> shader
  (** [shader ~loc:(`Loc (f, l)) ~inserts k src] is the shader of kind [k]
      made by pre-concatening the inserts [inserts] to [src] located 
      in file [f] at line [l]. If [loc] is unspecified the location 
      of the function call is used provided the program is compiled 
      with [-g]. *)

  val kind : shader -> shader_kind 
  (** [kind s] is the shader kind of [s]. *) 

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


(** Effects

    An effect defines a configuration of the graphics pipeline or 
    a list of effects for rendering a {!Geometry.t} value. *)
module Effect : sig


  (** {1 Rasterization state} 

      {b Note.} Faces with a counter clockwise orientation are front 
      faces. *) 
(*
  type cull = [ `None | `Front | `Back ] 
  type raster = { cull : cull } 
  val default_raster : raster
*)
  (** {1 Depth state} 


      {b Note.} Depth clearing and depth range are specified 
      in {Camera.t} values. *)

(*
  type depth_test = 
    [ `Never | `Less | `Equal | `Lequal | `Greater | `Nequal 
    | `Gequal | `Always ]

  type depth = { test : depth_test option; 
                 (** [Some _] if z-test should be performed (defaults to 
                     [Some `Lequal] *)
                 write : bool; 
                 (** [true] if z-buffer should be written (default). *)
                 offset : float * float; 
                 (** factor, units *) }
  (** The type for depth state *) 

  type default_depth : depth
*)

  type t = effect
  val create : prog -> effect   
  val prog : effect -> prog
end

(** {1 Rendering} *) 

type view
(** The type for rendered views. *)

(** Views 

    A view defines a viewing volume in space, the view's background 
    color and the rectangular portion of the renderer's target it will render
    to *) 
module View : sig
  type t = view
  val create : unit -> t
end

type renderer 
(** The type for renderers. *)

type op = 
  { count : int; effect : effect; prim : prim }
(** The type for render operations. *) 

(** Renderers *) 
module Renderer : sig

  (** {1 Logging} *) 

  module Log : sig

    (** {1 Compiler messages} *) 

    type compiler_msg = 
      [ `Msg of string | `Msg_loc of string * Prog.loc * string ]
    (** The type for compiler messages. *) 

    val pp_compiler_msg : Format.formatter -> compiler_msg -> unit 
    (** [pp_compiler_msg ppf m] prints an unspecified representation of [m]
        on [ppf]. *)
    
    type compiler_parser = string -> 
      [ `Loc of string * int * int * string | `Unparsed of string  ]
    (** The type for GPU compiler message parser. Parses a string into 
        four parts: before the file number, the file number, the line number, 
        after the line number. *) 
                                
    val compiler_parser_default : compiler_parser
    (** [compiler_parser_default] parses msg:int:int:msg or int:int:msg 
        into [`Loc]. *)
 
    val compiler_parser_raw : compiler_parser
    (** [compiler_parser_raw s] is always [`Unparsed], i.e. gets the 
        the raw log. *)

    (** {1 Renderer messages} *) 

    type msg = 
      [ `Compiler of compiler_msg list
      | `Linker of string list 
      | `Missing_attr of prim * string
      | `Unsupported_shaders of prog * Prog.shader list 
      | `Msg of string ]
    (** The type for log messages. *) 

    val pp_msg : Format.formatter -> msg -> unit 
    (** [pp_msg ppf m] prints an unspecified representation of [m] on 
        [ppf]. *) 

    (** {1:logs Logs} *) 

    type level = [ `Error | `Debug ] 
    (** The type for log levels. *)
    
    type t = level -> msg -> unit
    (** The type for logs. *) 

    val of_formatter : Format.formatter -> t 
    (** [of_formatter ppf] logs on [ppf]. *) 
  end

  (** Private functions for implementing renderers. 
  
      {b Warning.}  [Lit] users should not use these definitions. They 
      expose [Lit]'s internals and are subject to change even between 
      minor versions of the library. *)
  module Private : sig

    module Id : sig
      type t = int
      val compare : t -> t -> int
    end

    module Info : sig
      type t 
      val create : unit -> ('a -> t) * (t -> 'a option)
      val none : t
    end

    module Cap : sig 
      val parse_version : string -> (int * int * int) option 
    end

    module Buf : sig
      include module type of Buf
      val set_gpu_count : buf -> int -> unit 
      val set_gpu_exists : buf -> bool -> unit
      val cpu_byte_count : buf -> int 
      val cpu_p : buf -> ('a, 'b) bigarray option
      val check_kind : buf -> ('a, 'b) Bigarray.kind -> unit
      val info : buf -> Info.t
      val set_info : buf -> Info.t -> unit
    end
   
    module Attr : sig 
      include module type of Attr
    end

    module Prim : sig 
      include module type of Prim
      val info : prim -> Info.t
      val set_info : prim -> Info.t -> unit
    end

    module Prog : sig 
      include module type of Prog
      val info : prog -> Info.t
      val set_info : prog -> Info.t -> unit
    end

    module Effect : sig
      include module type of Effect
      val info : effect -> Info.t
      val set_info : effect -> Info.t -> unit
    end

    module Log : sig 
      include module type of Log 
      val split_string : char -> string -> string list 
      val lines : string -> string list           
      val compiler_msg : string -> compiler_parser ->
        (int * string) list -> [> `Compiler of compiler_msg list ]
    end
  end

  (** {1:renderers Renderers} *)

  (** The type for a renderer backend. *) 
  module type T = sig
    type t 
    val name : string
    val create : 
      ?log_compiler_parser:Log.compiler_parser -> 
      time:(unit -> float) -> Log.t -> debug:bool -> size2 -> t
    val size : t -> size2
    val set_size : t -> size2 -> unit
    val view : t -> View.t 
    val set_view : t -> View.t -> unit
    val frame_begin : t -> unit
    val frame_add : t -> op -> unit
    val frame_end : t -> unit
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
  
  type t = renderer 
  (** The type for renderers. *)

  val create : ?log_compiler_parser:Log.compiler_parser -> 
    ?time:(unit -> float) -> ?log:Log.t -> 
    ?debug:bool -> size:size2 -> (module T) -> renderer

  val size : renderer -> size2
  val set_size : renderer -> size2 -> unit
  val view : renderer -> View.t
  val set_view : renderer -> View.t -> unit
  val frame_begin : renderer -> unit
  val frame_add : renderer -> op -> unit
  val frame_end : renderer -> unit
  val release : renderer -> unit
  
  (** Renderer capabilities. *)
  module Cap : sig

    (** {1 Shader capabilities} *)

    val shader_kinds : renderer -> Prog.shader_kind list
    (** [shader_kinds r] is the list of {{!Prog.shader_kind}shader kinds} 
        supported by the renderer. *)

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

  (** Renderer specific buffer functions. *)
  module Buf : sig

    (** {1 Mapping GPU buffers in CPU} *) 

    val map : renderer -> [ `R | `W | `RW ] -> Buf.t 
      -> ('a, 'b) Bigarray.kind -> ('a, 'b) bigarray 
    (** [map r access b kind] maps the GPU buffer of [b] with 
        access [access].

        {b Warning.} A mapped buffer cannot be used in a render
        operation, you need to {!unmap} it first. Once unmapped the
        bigarray becomes invalid. Don't try to access it, it may result
        in program termination.

        @raise Invalid_argument if [kind] does not match [buf]'s
        scalar type. *)

    val unmap : renderer -> Buf.t -> unit
    (** [unmap r b] unmaps the buffer [b]. 

        {b Warning.} This invalidates the memory pointed to 
        by the bigarray returned by {!map}, do not try to 
        access it after this function has been called it may
        result in program termination. *)
  end

end

(** {1 Relationship to OpenGL contexts} 

    {ul 
    {- For now, [Buf.t], [Geom.t] and [Effect] value should not be shared
       across renderers.}
    {- Note about OpenGL ids represented as [int]s.}} *) 


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
