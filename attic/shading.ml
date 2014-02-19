(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

open Gg;;

type id = string
type op = 
    Not | Or | Xor | And | If | Eq | Neq | Gt | Leq | Lt |
    Neg | Add | Sub | Mul | Div | Sadd | Smul | Ssub | Sdiv | Apply | 
    Sel of swizzle list | 
    Sin | Asin | Cos | Acos | Tan | Atan | Atan2 | 
    Pow | Exp | Log | Exp2 | Log2 | Sqrt | Inverse_sqrt | 
    Abs | Sign | Floor | Ceil | Fract |
    Mod | Smod | Min | Smin | Max | Smax | Sclamp | Clamp | 
    Mix | Smix | Step | Sstep | Smooth_step | Ssmooth_step | 
    Length | Distance | Dot | Cross | Normalize| Face_forward | Reflect | 
    Matrix_cmult | Any | All | 
    Tex1 | Tex1_proj | Tex1_lod | Tex1_proj_lod | 
    Tex2 | Tex2_proj | Tex2_lod | Tex2_proj_lod | 
    Tex3 | Tex3_proj | Tex3_lod | Tex3_proj_lod | 
    Tex_cube | Tex_cube_lod |
    Depth1 | Depth1_proj | Depth1_lod | Depth1_proj_lod | 
    Depth2 | Depth2_proj | Depth2_lod | Depth2_proj_lod |
    Df_dx | Df_dy | Fwidth | Noise1 | Noise2 | Noise3 | Noise4


type dexp =                (* target language expressions, dynamically typed. *)
  | B1 of bool
  | B2 of bool * bool 
  | B3 of bool * bool * bool
  | B4 of bool * bool * bool * bool
  | I1 of int
  | I2 of int * int
  | I3 of int * int * int
  | I4 of int * int * int * int
  | F1 of float
  | F2 of v2
  | F3 of v3
  | F4 of v4
  | M2 of m2
  | M3 of m3
  | M4 of m4
  | Apply of op * (dexp list)
  | Var of id                              (* Only during cse and compilation *)
  | Let of (id * dexp) list * dexp                                   (* ditto *)

let var s = Var s
let apply1 op e1 = Apply (op, [e1])
let apply2 op e1 e2 = Apply (op, [e1; e2])
let apply3 op e1 e2 e3 = Apply (op, [e1; e2; e3])
let applyn op exps = Apply (op, exps)     

module Dumb = struct 
  let not' b = apply1 Not b
  let or'  b b' = apply2 Or b b'
  let xor' b b' = apply2 Xor b b'
  let and' b b' = apply2 And b b'
  let if'  b e e' = apply3 If b e e'

  let eq  v v' = apply2 Eq v v'  
  let neq v v' = apply2 Neq v v'  
  let geq v v' = apply2 Geq v v'  
  let gt  v v' = apply2 Gt v v'  
  let leq v v' = apply2 Leq v v'  
  let lt  v v' = apply2 Lt v v'  

  let neg v = apply1 Eq v
  let add v v' = apply2 Add v v'  
  let sub v v' = apply2 Sub v v'  
  let mul v v' = apply2 Mul v v'  
  let div v v' = apply2 Div v v'   

  let sadd s v = apply2 Sadd s v
  let smul s v = apply2 Smul s v
  let ssub s v = apply2 Ssub s v
  let sdiv s v = apply2 Sdiv s v
  
  let apply m v = apply2 Apply m v

  let sin' v = apply1 Sin v
  let asin' v = apply1 Asin v
  let cos' v = apply1 Cos v
  let acos' v = apply1 Acos v
  let tan' v = apply1 Tan v
  let atan' v = apply1 Atan v
  let atan2' v v' = apply1 Atan2 v v'

(** Exponential functions *)
  let pow v = apply1 Pow v 
  let exp' v = apply1 Exp v
  let log' v = apply1 Log v
  let exp2 v = apply1 Exp2 v
  let log2 v = apply1 Log2 v
  let sqrt' v = apply1 Sqrt v
  let inverse_sqrt v = apply1 Inverse_sqrt v

(** Common functions *)
  let abs' v = apply1 Pow v 
  let sign v = apply1 Pow v 
  let floor' v = apply1 Pow v 
  let ceil' v = apply1 Pow v 
  let fract v = apply1 Pow v 
  let mod' v v' = apply2 Pow v v' 
  let smod v s  = apply2 Pow v s  
  let min' v v' = apply2 Min v v'
  let smin s v =  apply2 Smin v v'
  let max' v v'= 
  let smax s v = 
  let sclamp s v = 
  let clamp v = 
  let mix v = 
  let smix v= 
  let step ~edge v = 
  let sstep ~edge v = 
let smoothstep ~edge0 ~edge1 v =
let ssmoothstep ~edge0 ~edge1 v = 

(** Geometric functions *)
let length   = 'a fv_e -> float_e
let distance = 'a fv_e -> 'a fv_e -> float_e
let dot      = 'a fv_e -> 'a fv_e -> float_e
let cross    = [`D3] fv_e -> [`D3] fv_e -> [`D3] fv_e
let normalize = 'a fv_e -> 'a fv_e -> 'a fv_e 
(* ftransform = unit -> `D4 fv_e *)
let faceforward = n='a fv_e -> 'a fv_e -> nref='a fv_e -> 'a fv_e
let reflect = 'a fv_e -> n='a fv_e -> 'a fv_e 

(** Matrix functions *)
let matrix_cmult = 'a m_e -> 'a m_e -> 'a m_e

(** Vector relational functions *)
let any = 'a bv_e -> bool_e
let all = 'a bv_e -> bool_e

(** Texture access functions, 

   bias is ignored in vertex programs *)

let tex1      = ?bias=float_e -> sampler1 -> float_e -> [`D4] fv_e 
let tex1_proj = ?bias=float_e -> sampler1 -> [`D2 | `D4] fv_e -> [`D4] fv_e 
let tex1_lod  = sampler1 -> float_e -> lod=float_e -> [`D4] fv_e 
let tex1_proj_lod = sampler1 -> [`D2 | `D4] fv_e -> lod=float_e -> [`D4] fv_e 

let tex2      = ?bias=float_e -> sampler2 -> [`D2] fv_e -> [`D4] fv_e 
let tex2_proj = ?bias=float_e -> sampler2 -> [`D3 | `D4] fv_e -> [`D4] fv_e 
let tex2_lod  = sampler2 -> [`D2] fv_e -> lod=float_e -> [`D4] fv_e 
let tex1_proj_lod = sampler2 -> [`D3 | `D4] fv_e -> lod=float_e -> [`D4] fv_e 

let tex3      = ?bias=float_e -> sampler3 -> [`D3] fv_e -> [`D4] fv_e 
let tex3_proj = ?bias=float_e -> sampler3 -> [`D4] fv_e -> [`D4] fv_e 
let tex3_lod  = sampler3 -> [`D3] fv_e -> lod=float_e -> [`D4] fv_e 
let tex3_proj_lod = sampler3 -> [`D4] fv_e -> lod=float_e -> [`D4] fv_e 

let tex_cube = ?bias=float_e -> sampler_cube -> [`D3] fv_e -> [`D3] fv_e
let tex_cube_lod = sampler_cube -> [`D3] fv_e -> lod=float_e -> [`D3] fv_e

let depth1 = ?bias=float_e -> sampler_depth1 -> [`D3] fv_e -> [`D4] fv_e 
let depth1_proj = ?bias=float_e -> sampler_depth1 -> [`D4] fv_e -> [`D4] fv_e 
let depth1_lod  = sampler_depth1 -> [`D3] fv_e -> lod=float_e -> [`D4] fv_e 
let depth1_lod_proj  = sampler_depth1 -> [`D4] fv_e -> lod=float_e -> 
  [`D4] fv_e 

let depth2 = ?bias=float_e -> sampler_depth2 -> [`D4] fv_e -> [`D4] fv_e 
let depth2_proj = ?bias=float_e -> sampler_depth2 -> [`D4] fv_e -> [`D4] fv_e 
let depth2_lod  = sampler_depth2 -> [`D3] fv_e -> lod=float_e -> [`D4] fv_e 
let depth2_lod_proj  = sampler_depth2 -> [`D4] fv_e -> lod=float_e -> 
  [`D4] fv_e 

(** Fragment processing function, use only in fragment shaders. *)
let dF_dx = 'a fv_e -> 'a fv_e
let dF_dy = 'a fv_e -> 'a fv_e
let fwidth = 'a fv_e -> 'a fv_e 

(** Noise *)
let noise1 = 'a fv_e -> float_e
let noise2 = 'a fv_e -> [`D2] fv_e
let noise3 = 'a fv_e -> [`D3] fv_e
let noise4 = 'a fv_e -> [`D4] fv_e



end

module Smart = struct
end



type size     = [ `D1 | `D2 | `D3 | `D4 ]
type el       = [ `Bool | `Int | `Float ]
type num   = [ `Int | `Float ]
type kind     = [ `V | `M ]

type 'a exp
(** Expressions of the shading language. ['a] denotes the type of 
 the value computed by the expression. *)

type 'a scalar_e = ('a * [`D1] * [`V]) exp
type bool_e  = [`Bool]  scalar_e
type int_e   = [`Int]   scalar_e
type float_e = [`Float] scalar_e

type ('a, 'b) v_e = ('a * 'b * [`V]) exp
type 'a bv_e = ([`Bool], 'a) v_e
type 'a iv_e = ([`Int], 'a) v_e
type 'a fv_e = ([`Float], 'a) v_e
type 'a v1_e = ('a, [`D1]) v_e
type 'a v2_e = ('a, [`D2]) v_e
type 'a v3_e = ('a, [`D3]) v_e
type 'a v4_e = ('a, [`D4]) v_e

type 'a m_e = ([`Float] * 'a * [`M]) exp
type m2_e = [`D2] m_e
type m3_e = [`D3] m_e
type m4_e = [`D4] m_e


type sampler1
type sampler2
type sampler3
type sampler_cube
type sampler_depth1
type sampler_depth2

type 'a arr

val bool : bool -> bool_e
val int : int -> int_e
val float : float -> float_e

val bv1 : bool -> [`Bool] v1_e
val bv2 : bool -> bool -> [`Bool] v2_e
val bv3 : bool -> bool -> bool -> [`Bool] v3_e
val bv4 : bool -> bool -> bool -> bool -> [`Bool] v4_e

val iv1 : int -> [`Int] v1_e
val iv2 : int -> int -> [`Int] v2_e
val iv3 : int -> int -> int -> [`Int] v3_e
val iv4 : int -> int -> int -> int -> [`Int] v4_e

val fv1 : float -> [`Float] v1_e
val fv2 : float -> float -> [`Float] v2_e
val fv3 : float -> float -> float -> [`Float] v3_e
val fv4 : float -> float -> float -> float -> [`Float] v4_e

val v1 : float -> [`Float] v1_e
val v2 : float -> float -> [`Float] v2_e
val v3 : float -> float -> float -> [`Float] v3_e
val v4 : float -> float -> float -> float -> [`Float] v4_e

val m2 : float -> float -> float -> float -> m2_e
val m3 : float -> float -> float -> float -> float -> float -> 
  float -> float -> float -> m3_e
val m4 : float -> float -> float -> float -> float -> float -> float -> 
  float -> float -> float -> float -> float -> float -> float -> float -> 
    float -> m4_e

val s :  ('a, 'b) v_e -> int_e -> 'a v1_e
(* check usefulness with iterators. *)

val col : 'a m_e -> int_e -> 'a fv_e 
val el : 'a m_e  -> int_e -> int_e -> float_e


val s1 : ('a, 'b) v_e -> sel -> 'a v1_e 
val s2 : ('a, [< `D2 | `D3 | `D4]) v_e -> sel -> sel -> 'a v2_e
val s3 : ('a, [< `D3 | `D4]) v_e -> sel -> sel -> sel -> 'a v3_e
val s4 : 'a v4_e -> sel -> sel -> sel -> sel -> 'a v4_e


(* TODO add constructions of matrices by vectors *)


module Override : sig
  let not = not'
  let ( && ) = and'
  let ( || ) = or'

  let ( = )  = 'a exp -> 'a exp -> bool_e
  let ( <> )  = 'a exp -> 'a exp -> bool_e

  let ( = )  = eq
  let ( <> ) = neq
  let ( >= ) = geq
  let ( > )  = gt
  let ( <= ) = leq
  let ( < )  = lt

  let ( + )  = add'
  let ( - )  = sub'
  let ( * )  = mul'
  let ( / )  = div'

  let ( <*> ) = apply

  let sin   = sin'
  let asin  = asin'
  let cos   = cos'
  let acos  = acos'
  let tan   = tan'
  let atan  = atan'
  let atan2 = atan2'
      
  let ( ** )  = pow
  let exp     = exp'
  let log     = log'
  let sqrt    = sqrt'

  let abs   = abs'
  let ceil  = ceil'
  let ( mod )   = mod'
  let min   = min'
  let max   = max'
  let ( <.> ) = dot

  let ( <%> ) = cross
end


