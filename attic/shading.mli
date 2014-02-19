(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)


(** Shader programming. 

Open this module to use it.

{b Pervasives overridings.}  This module does not redefine any
operator or function from [Pervasives]. Functions with the same name
like [log] are primed ([log']) to avoid redefinitions.  However for
readability it is advised to open {!Shading.Override}, which overrides
arithmetic operators and functions.

{b Note.}  Documentation of certain functions is scarce. For more
informations consult {{:http://oss.sgi.com/projects/ogl-sample/registry/ARB/GLSLangSpec.Full.1.10.59.pdf}documentation} on {e GLSL}, the mapping is obvious.
*)

open Gg;;

(** The values one manipulates when writing shaders are {e
expressions} (or program fragments), hence the term {e
meta-programming}. All expressions are represented by the type below 
whose parameter denote the type of the value computed by the
program fragment. *)

type 'a exp
(** Expressions of the underlying shading language. ['a] denotes the
 type of the value computed by the expression. *)

(** To precisely describe in ['a] the type of the value computed by an
   expression we use the following types. *)

type dim = [ `D1 | `D2 | `D3 | `D4 ]
(** Type to specify dimensions. *)

type storage = [ `Bool | `Int | `Float ]
(** Type to specify storage types. *)

type num = [ `Int | `Float ]
(** Numerical storage, a subtype of {!Shading.storage}. *)

type kind = [ `V | `M ]
(** Type to specify the kind of data, respectively vectors and matrices. *)

(** An expression with type [('a * 'b * 'c) exp], describes a value
   whose storage type is ['a], dimension is ['b] and kind is ['c] *)

(** {1:typ Basic datatypes} *)

(** 
   There are two kind of basic datatypes. 
   {ol 
   {- Those that can be computed by your expressions, essentially scalars, 
      vector and matrices.}
   {- Those that cannot be computed by your expressions but that you can use
   with primitive functions to get a computable value, for example samplers.}}

   In the first case, when we talk about the value of a datatype, we
   refer in fact to an expression computing a value of this
   datatype. However, conceptually, you should abstract from that
   detail and treat it as if it was a value of that type.

   Types that represent expressions are primed. For example [float']
   is an expression denoting a [float] (i.e. [([`Float] * [`D1] *
   [`V]) exp)], whereas ['a fv'] is an expression denoting a vector of
   floats of any dimension (i.e. [([`Float] * 'a * [`V]) exp]).  *)

(** {2:scal Scalars} 
   Scalars are equivalents to 1D vectors.
*)

type 'a scalar' = ('a * [`D1] * [`V]) exp
(** The type for scalars, 'a denotes the storage type. *)
type bool' = [`Bool] scalar'
(** The type for booleans. *)
type int' = [`Int] scalar'
(** The type for integers. *)
type float' = [`Float] scalar'
(** The type for floats. *)

(** {3:sc Scalar constant constructors} 
These functions allow you to specify scalar constants in shaders. 
*)

val bool : bool  -> bool'
val int : int   -> int'
val float : float -> float'

(** {3:sc Scalar conversion} 
These functions allow you to convert between scalar types. 
When a boolean is converted to a numeral, [true]
maps to [1] and [false] to [0]. When a numeral is converted to a
boolean, anything but [0] maps to [true].  *)

val to_bool' : 'a scalar' -> bool'
val to_int' : 'a scalar' -> int'
val to_float' : 'a scalar' -> float'

(** {2:vect Vectors} *)

type ('a, 'b) v' = ('a * 'b * [`V]) exp
(** The type for vectors. 
   'a denotes the storage type, 'b denotes the dimension. *)

type 'a v1' = ('a, [`D1]) v'
(** 1D vectors, 'b denotes the storage. *)
type 'a v2' = ('a, [`D2]) v'
(** 2D vectors, 'b denotes the storage. *)
type 'a v3' = ('a, [`D3]) v'
(** 3D vectors, 'b denotes the storage. *)
type 'a v4' = ('a, [`D4]) v'
(** 4D vectors, 'b denotes the storage. *)

type 'a bv' = ([`Bool], 'a) v'
(** Boolean vectors, 'a denotes the dimension. *)

type 'a iv' = ([`Int], 'a) v'
(** Integer vectors, 'a denotes the dimension. *)

type 'a fv' = ([`Float], 'a) v'
(** Float vectors, 'a denotes the dimension. *)

(** {3:vc Vector constructors} Vector constructors come in primed and
unprimed versions. The latter allow you to conveniently construct
constants ones from ocaml. Some functions are also provided to directly
interface with Gg. *)

val bv2 : bool -> bool -> [`Bool] v2'
val bv2' : bool' -> bool' -> [`Bool] v2'
val bv3 : bool -> bool -> bool -> [`Bool] v3'
val bv3' : bool' -> bool' -> bool' -> [`Bool] v3'
val bv4 : bool -> bool -> bool -> bool -> [`Bool] v4'
val bv4' : bool' -> bool' -> bool' -> bool' -> [`Bool] v4'

val iv2 : int -> int -> [`Int] v2'
val iv2' : int' -> int' -> [`Int] v2'
val iv3 : int -> int -> int -> [`Int] v3'
val iv3' : int' -> int' -> int' -> [`Int] v3'
val iv4 : int -> int -> int -> int -> [`Int] v4'
val iv4' : int' -> int' -> int' -> int' -> [`Int] v4'

val fv2 : float -> float -> [`Float] v2'
val fv2' : float' -> float' -> [`Float] v2'
val fv3 : float -> float -> float -> [`Float] v3'
val fv3' : float' -> float' -> float' -> [`Float] v3'
val fv4 : float -> float -> float -> float -> [`Float] v4'
val fv4' : float' -> float' -> float' -> float' -> [`Float] v4'

val of_v2 : v2 -> [`Float] v2'
val of_v3 : v3 -> [`Float] v3'
val of_v4 : v4 -> [`Float] v4'
val of_quat : quat -> [`Float] v4'

(** {3:vd Vector deconstructors} *)

(** The following functions allow to convert between vectors. *)

val to_scalar' : ('a, [< `D2 | `D3 | `D4]) v'-> 'a scalar'
val to_v1' : ('a, [< `D2 | `D3 | `D4]) v'-> 'a scalar'
val to_v2' : ('a, [< `D3 | `D4]) v' -> 'a v2'
val to_v3' : 'a v4' -> 'a v3'

(** The functions below allow to select components of vector
symbolically ({e swizzling}) or numerically. Note that it is legal
when you select more than one component to specify the same component
more than once (e.g. [let xx = sv2 v `X `X]). *)

type sel = [ `X | `Y | `Z | `W | 
             `R | `G | `B | `A | 
	     `S | `T | `P | `Q  ]
(** This type specifies a particular component of a vector
   symbolically. The first, second, third and fourth can respectively
   be specified either with [`X, `Y, `Z] and [`W], or [`R, `G, `B] and [`A,] or
   [`S, `T, `P] and [`Q]. *)

val sv1 : ('a, 'b) v' -> sel -> 'a v1' 
val sv2 : ('a, [< `D2 | `D3 | `D4]) v' -> sel -> sel -> 'a v2'
val sv3 : ('a, [< `D3 | `D4]) v' -> sel -> sel -> sel -> 'a v3'
val sv4 : 'a v4' -> sel -> sel -> sel -> sel -> 'a v4'

val isv1 : ('a, 'b) v' -> int  -> 'a v1'
val isv1' : ('a, 'b) v' -> int' -> 'a v1'

(** {2:mat Matrices} *)

type 'a fm' = ([`Float] * 'a * [`M]) exp
(** The type for matrices. *)
type fm2' = [`D2] fm'
(** The type for 2x2 matrices. *)
type fm3' = [`D3] fm'
(** The type for 3x3 matrices. *)
type fm4' = [`D4] fm'
(** The type for 4x4 matrices. *)

(** {3:mc Matrix constructors} 
Matrix constructors come in primed and
unprimed versions. The latter allow you to conveniently construct
constants ones from ocaml. Some functions are also provided to directly
interface with Gg. 

In the following functions, matrices are specified in {e row} order.  *)

val fm2  : float  -> float  -> float  -> float   -> fm2'
val fm2' : float' -> float' -> float' -> float' -> fm2'
val fm3  : float  -> float  -> float  -> 
           float  -> float  -> float  -> 
           float  -> float  -> float  -> fm3'
val fm3' : float' -> float' -> float' -> 
           float' -> float' -> float' -> 
           float' -> float' -> float  -> fm3'
val fm4  : float  -> float  -> float  -> float  -> 
           float  -> float  -> float  -> float  -> 
           float  -> float  -> float  -> float  -> 
           float  -> float  -> float  -> float  -> fm4'
val fm4' : float' -> float' -> float' -> float' ->
           float' -> float' -> float' -> float' ->
           float' -> float' -> float' -> float' ->
           float' -> float' -> float' -> float' -> fm4'

val of_m2 : m2 -> fm2'
val of_m3 : m3 -> fm3'
val of_m4 : m4 -> fm4' 

(** The following functions specify a matrix by its {e columns}. *)

val vfm2' : [`Float] v2' -> [`Float] v2' -> fm2'  
val vfm3' : [`Float] v3' -> [`Float] v3' -> [`Float] v3' -> fm3'  
val vfm4' : [`Float] v4' -> [`Float] v4' -> [`Float] v4' -> [`Float] v4' -> fm4'



(** {3:md Matrix deconstructors} 
   Matrices are accessed in {e column}, {e row} order.  
*)

val col : 'a fm' -> int -> 'a fv' 
val col' : 'a fm' -> int' -> 'a fv' 
val el : 'a fm' -> col:int -> row:int -> float'
val el' : 'a fm' -> col:int' -> row:int' -> float'


(** {2:vect Samplers} 
TODO proper doc. Samplers are used to lookup texture data via
certain functions. They cannot be computed by an expression.  *)

type sampler1
(** 1D texture lookup. *)
type sampler2
(** 2D texture lookup. *)
type sampler3
(** 3D texture lookup. *)
type sampler_cube
(** Cube-mapped texture lookup. *)
type sampler_depth1
(** 1D depth texture lookup. *)
type sampler_depth2
(** 2D depth texture lookup. *)

(** {2:cont Containers} *)

type 'a container
(** The size of container must be known at compilation time. *)


val nth : 'a container -> 'a exp 
val fold_left : ('a exp -> 'b exp -> 'a exp) -> 'a exp -> 'b container -> 'a exp
val fold_right : ('a exp -> 'b exp -> 'b exp) -> 'b container -> 'b exp -> 
'b exp
(** TODO 
val accumi : (int' -> int' -> 'a exp -> 'b exp) -> *)

(** {2:vect Attributes} 
   Attributes only in vertex shaders and are limited to [`Float] * 'b * 'c). 
   Uniform same across primitive. 

   Varying, interface between vertex sh. and frag sh.
   *)


(** {1:funs Functions and operators} *)

   (** [Pervasives] operator and function overrides. *)
module Override : sig
  val not : 'a bv' -> 'a bv'
  (** See {!Shading.not'}. *)
  val ( && ) : bool' -> bool' -> bool'
  (** See {!Shading.and'}. *)
  val ( || ) : bool' -> bool' -> bool'
  (** See {!Shading.or'}. *)
  val ( = ) : ('a, 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.eq}. *)
  val ( <> ) : ('a, 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.neq}. *)
  val ( >= ) : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.geq}. *)
  val ( > ) : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.gt}. *)
  val ( <= ) : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.leq}. *)
  val ( < )  : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
  (** See {!Shading.lt}. *)
  val ( ~- ) : (([< num] * 'b * 'c) as 'a) exp -> 'a exp
  (** See {!Shading.neg}. *)
  val ( + ) : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
  (** See {!Shading.add}. *)
  val ( - ) : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
  (** See {!Shading.sub}. *)
  val ( * ) : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
  (** See {!Shading.mul}. *)
  val ( / ) : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
  (** See {!Shading.div}. *)

  val sin : 'a fv' -> 'a fv'  
  (** See {!Shading.sin'}. *)    
  val asin : 'a fv' -> 'a fv'  
  (** See {!Shading.asin'}. *)    
  val cos : 'a fv' -> 'a fv'  
  (** See {!Shading.cos'}. *)    
  val acos : 'a fv' -> 'a fv' 
  (** See {!Shading.acos'}. *)    
  val tan : 'a fv' -> 'a fv' 
  (** See {!Shading.tan'}. *)   
  val atan : 'a fv' -> 'a fv'
  (** See {!Shading.atan'}. *)  
  val atan2 : 'a fv' -> 'a fv' -> 'a fv'
  (** See {!Shading.atan2'}. *)

  val ( ** ) : 'a fv' -> 'a fv' -> 'a fv'
  (** See {!Shading.pow}. *)
  val exp : 'a fv' -> 'a fv'
  (** See {!Shading.exp'}. *)
  val log : 'a fv' -> 'a fv'
  (** See {!Shading.log'}. *)
  val sqrt : 'a fv' -> 'a fv'
  (** See {!Shading.sqrt'}. *)
  val abs : 'a fv' -> 'a fv'
  (** See {!Shading.abs'}. *)
  val ceil : 'a fv' -> 'a fv' 
  (** See {!Shading.ceil'}. *)
  val ( mod ) : 'a fv' -> 'a fv' -> 'a fv'
  (** See {!Shading.mod'}. *)
  val min : 'a fv' -> 'a fv' -> 'a fv'
  (** See {!Shading.min'}. *)
  val max : 'a fv' -> 'a fv' -> 'a fv'
  (** See {!Shading.max'}. *)
  val ( <.> ) : 'a fv' -> 'a fv' -> float'
  (** See {!Shading.dot}. *)
  val ( <%> ) : [`D3] fv' -> [`D3] fv' -> [`D3] fv'
  (** See {!Shading.cross}. If you don't see the symbol here, it is [<%>].*)
end


(** {3:bo Boolean operators and conditionals} *) 

val not' : 'a bv' -> 'a bv'
(** Component wise logical not. *)
val any : 'a bv' -> bool'
(** [true] if any component is [true] *)
val all : 'a bv' -> bool'
(** [true] if all components are [true] *)

val or' : bool' -> bool' -> bool'
(** Lazy evaluation of second argument. *)
val xor' : bool' -> bool' -> bool'
val and' : bool' -> bool' -> bool'
(** Lazy evaluation of second argument. *)
val if' : bool' -> 'a exp -> 'a exp -> 'a exp

(** {3:ro Relational operators} 
 The following functions operate component wise.
*)

val eq : ('a, 'b) v' -> ('a, 'b) v' -> 'b bv'
val neq : ('a, 'b) v' -> ('a, 'b) v' -> 'b bv'
val geq : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
val gt : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
val leq : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'
val lt : (([< num] as 'a), 'b) v' -> ('a, 'b) v' -> 'b bv'

(** {3:ao Arithmetic operators } 

The following functions operate component wise on numerical vector and
matrices {b except} for {!Shading.mul} which performs linear algebraic
multiplication on matrices. Use {!Shading.fm_cmul} for component wise
multiplication of matrices and {!Shading.apply} for linear algebraic
multiplication of a vector by a matrix. *)

val neg : (([< num] * 'b * 'c) as 'a) exp -> 'a exp
val add : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
val sub : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp
val mul : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 
val div : (([< num] * 'b * 'c) as 'a) exp -> 'a exp -> 'a exp 

val sadd : ('a * 'b * 'c) exp -> ([< num] as 'a) scalar' -> 
  ('a * 'b * 'c) exp
val smul : ([< num] as 'a) scalar' -> ('a * 'b * 'c) exp  ->
  ('a * 'b * 'c) exp
(** Arguments are reversed to match usual notation in mathematics
   and in {!}. *)
val ssub : ('a * 'b * 'c) exp -> ([< num] as 'a) scalar' ->
  ('a * 'b * 'c) exp
val sdiv : ('a * 'b * 'c) exp -> ([< num] as 'a) scalar' -> 
  ('a * 'b * 'c) exp

val fm_cmul : 'a fm' -> 'a fm' -> 'a fm'
(** Component-wise multiplication of a matrix. *)

val apply : 'a fm' -> 'a fv' -> 'a fv'
(** Linear algbraic multiplication of a vector by a matrix. *)


(** {3:sc Trigonometric functions } *)

val sin' : 'a fv' -> 'a fv'  
val asin' : 'a fv' -> 'a fv'  
val cos' : 'a fv' -> 'a fv'  
val acos' : 'a fv' -> 'a fv'  
val tan' : 'a fv' -> 'a fv'  
val atan' : 'a fv' -> 'a fv'  
val atan2' : 'a fv' -> 'a fv' -> 'a fv'

(** Exponential functions *)
val pow : 'a fv' -> 'a fv' -> 'a fv'
val exp' : 'a fv' -> 'a fv'
val log' : 'a fv' -> 'a fv'
val exp2 : 'a fv' -> 'a fv'
val log2 : 'a fv' -> 'a fv'
val sqrt' : 'a fv' -> 'a fv'
val inverse_sqrt : 'a fv' -> 'a fv'

(** Common functions *)
val abs' : 'a fv' -> 'a fv'
val sign : 'a fv' -> 'a fv'
val floor' : 'a fv' -> 'a fv'
val ceil' : 'a fv' -> 'a fv' 
val fract : 'a fv' -> 'a fv' 
val mod' : 'a fv' -> 'a fv' -> 'a fv'
val smod : 'a fv' -> float' -> 'a fv'
val min' : 'a fv' -> 'a fv' -> 'a fv'
val smin : 'a fv' -> float' -> 'a fv'
val max' : 'a fv' -> 'a fv' -> 'a fv'
val smax : 'a fv' -> float' -> 'a fv'
val sclamp : 'a fv' -> min:float' -> max:float' -> 'a fv' 
val clamp : 'a fv' -> min:float' -> min:float' -> 'a fv'
val mix : 'a fv' -> 'a fv' -> a:'a fv' -> 'a fv' 
val smix : 'a fv' -> 'a fv' -> a:float' -> 'a fv' 
val step : edge:'a fv' -> 'a fv' -> 'a fv'
val sstep : edge:float' -> 'a fv' -> 'a fv'
val smooth_step : edge0:'a fv' -> edge1:'a fv' -> 'a fv' -> 'a fv'
val ssmooth_step : edge0:float' -> edge1:float' -> 'a fv' -> 'a fv'

(** Geometric functions *)
val length : 'a fv' -> float'
val distance : 'a fv' -> 'a fv' -> float'
val dot : 'a fv' -> 'a fv' -> float'
val cross : [`D3] fv' -> [`D3] fv' -> [`D3] fv'
val normalize : 'a fv' -> 'a fv' -> 'a fv' 
(* ftransform : unit -> `D4 fv' *)
val face_forward : n:'a fv' -> 'a fv' -> nref:'a fv' -> 'a fv'
val reflect : 'a fv' -> n:'a fv' -> 'a fv' 

(** Matrix functions *)

(** Vector relational functions *)

(** Texture access functions, 

   bias is ignored in vertex programs *)

val tex1 : ?bias:float' -> sampler1 -> float' -> [`D4] fv' 
val tex1_proj : ?bias:float' -> sampler1 -> [`D2 | `D4] fv' -> [`D4] fv' 
val tex1_lod : sampler1 -> float' -> lod:float' -> [`D4] fv' 
val tex1_proj_lod : sampler1 -> [`D2 | `D4] fv' -> lod:float' -> [`D4] fv' 

val tex2 : ?bias:float' -> sampler2 -> [`D2] fv' -> [`D4] fv' 
val tex2_proj : ?bias:float' -> sampler2 -> [`D3 | `D4] fv' -> [`D4] fv' 
val tex2_lod : sampler2 -> [`D2] fv' -> lod:float' -> [`D4] fv' 
val tex1_proj_lod : sampler2 -> [`D3 | `D4] fv' -> lod:float' -> [`D4] fv' 

val tex3 : ?bias:float' -> sampler3 -> [`D3] fv' -> [`D4] fv' 
val tex3_proj : ?bias:float' -> sampler3 -> [`D4] fv' -> [`D4] fv' 
val tex3_lod : sampler3 -> [`D3] fv' -> lod:float' -> [`D4] fv' 
val tex3_proj_lod : sampler3 -> [`D4] fv' -> lod:float' -> [`D4] fv' 

val tex_cube : ?bias:float' -> sampler_cube -> [`D3] fv' -> [`D3] fv'
val tex_cube_lod : sampler_cube -> [`D3] fv' -> lod:float' -> [`D3] fv'

val depth1 : ?bias:float' -> sampler_depth1 -> [`D3] fv' -> [`D4] fv' 
val depth1_proj : ?bias:float' -> sampler_depth1 -> [`D4] fv' -> [`D4] fv' 
val depth1_lod : sampler_depth1 -> [`D3] fv' -> lod:float' -> [`D4] fv' 
val depth1_lod_proj : sampler_depth1 -> [`D4] fv' -> lod:float' -> 
  [`D4] fv' 

val depth2 : ?bias:float' -> sampler_depth2 -> [`D4] fv' -> [`D4] fv' 
val depth2_proj : ?bias:float' -> sampler_depth2 -> [`D4] fv' -> [`D4] fv' 
val depth2_lod : sampler_depth2 -> [`D3] fv' -> lod:float' -> [`D4] fv' 
val depth2_lod_proj : sampler_depth2 -> [`D4] fv' -> lod:float' -> 
  [`D4] fv' 

(** Fragment processing function, use only in fragment shaders. *)
val dF_dx : 'a fv' -> 'a fv'
val dF_dy : 'a fv' -> 'a fv'
val fwidth : 'a fv' -> 'a fv' 

(** Noise *)
val noise1 : 'a fv' -> float'
val noise2 : 'a fv' -> [`D2] fv'
val noise3 : 'a fv' -> [`D3] fv'
val noise4 : 'a fv' -> [`D4] fv'

(** {3:sf Special values and constants.} *)


val break : unit -> 'a exp
(** Allows to exit from an accumulator running on a container. *)

(** {3:sf Special vertex shader values and constants.} 
{b Warning.} Using any of these values or functions in a fragment shader
   will result in a compile time error. *)

type vert_result

val vert_position : [`Float] v4' -> vert_result exp
(** Specifies the the homogeneous vertex position to be used by
rasterization. {b Note.} Must be called in the vertex shader.*)

val vert_point_size : float' -> vert_result exp
(** Specifies the size, in pixels, of the point to be rasterized. *)

val vert_clip : [`Float] v4' -> vert_result exp
(** TODO gl_ClipVertex, if there is clip plane support. *)

(** {3:sf Special fragment shader values and constants.} 
{b Warning.} Using any of these values or functions in a vertex shader
   will result in a compile time error. *)

val frag_front_facing : bool' 
(** True if the fragment belongs to a front-facing primitive. *)


val frag_discard : unit -> 'a exp
(** This function abandons the operation on the current fragment. No update to 
   any buffer does occur.  *)

type frag_result 

val frag_coord : [`Float] v4' 
(** Window relative coordinates of the fragment. Results from the fixed
   functionality that interpolates primitives afer vertex processing to 
   generate fragments. `Z component contains the depth value as
   modified by the polygon offset calculation TODO link. *)




val frag_depth :  float' -> frag_result exp
(** Use this function to specify the fragment depth value that will be
   used in the subsequent fixed functionality pipeline.  If depth
   buffering is enabled and the shader does not return a value then
   the fixed functionality depth stored in [(sv1 frag_coord
   `Z)] is used by the pipeline. *)

val frag_color : [`Float] v4'  -> frag_result exp
(** Use this function to specify the fragment color that will be used
   in the subsequent fixed functionality pipeline. *)


(* Needs ARB_draw_buffers (multiple render target)
val frag_data   : [`Float] v4'  -> int  -> frag_result exp
val frag_data'  : [`Float] v4'  -> int' -> frag_result exp 
*)

(* attribute uniform varying *)
(* coercions *)
(* iteration : for, while, do while *)
(* jumps : discard, return, break, continue *)
