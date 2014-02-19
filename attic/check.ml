(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)


module Safe = struct
  let pr = Format.sprintf
  let inv = invalid_arg 
	   
  let eq f a v v' = if v <> v' then inv (pr "%s (%s) not = %s" a (f v) (f v')) 
  let gt f a v l = if v <= l then inv (pr "%s (%s) not > %s" a (f v) (f l))
  let geq f a v l = if v < l then inv (pr "%s (%s) not >= %s" a (f v) (f l))
  let lt f a v u = if v >= u then inv (pr "%s (%s) not < %s" a (f v) (f u))
  let leq f a v u = if v > u then inv (pr "%s (%s) not < %s" a (f v) (f u))
  let range f a v l u = if v < l || u < v then 
    inv ((pr "%s (%s) not in [%s;%s]") a (f v) (f l) (f u))

  let ieq a v v' = if v <> v' then inv (pr "%s (%d) not = %d" a v v')
  let igt a v l = if v <= l then inv (pr "%s (%d) not > %d" a v l)
  let igeq a v l = if v < l then inv (pr "%s (%d) not >= %d" a v l)
  let ilt a v u = if v >= u then inv (pr "%s (%d) not < %d" a v u)
  let ileq a v u = if v > u then inv (pr "%s (%d) not <= %d" a v u)
  let irange a v l u = 
    if v < l || u < v then inv (pr "%s (%d) not in [%d;%d]" a v l u)

  let is_range = function
    | None -> () 
    | Some (a,b) -> 
	if (a < 0 || b < 0 || b < a) then inv (pr "invalid range (%d,%d)" a b)
	    
  let spec f k k' = 
    if k <> k' then
      inv (pr "specialisation error, value is %s cast is %s" (f k) (f k'))
end

module Unsafe = struct
  let eq f a v v' = ()
  let gt f a v l = ()
  let geq f a v l = ()
  let lt f a v u = ()
  let leq f a v u = ()
  let range f a v l u = ()

  let ieq a v v' = ()
  let igt a v l = ()
  let igeq a v l = ()
  let ilt a v u = ()
  let ileq a v u = ()
  let irange a v l u = ()

  let is_range r = ()
  let spec f k k' = ()
end

include Safe
