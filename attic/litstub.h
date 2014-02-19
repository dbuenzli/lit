/*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*/

#ifndef _lit_h_
#define _lit_h_

#include <string.h>

#ifdef LIT_MACOSX
#include <mach-o/dyld.h>    /* For function ptrs */
#endif

#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/signals.h>

#include "litgl.h"

#define _err_str(s) #s
#define _lit_err_str(s,file,line) file ":" _err_str(line) ": " s
#define _lit_err(s) _lit_err_str(s,__FILE__,__LINE__)

/* Errors strings returned to ocaml */
#define LIT_ERR_UNIMPLEMENTED _lit_err("unimplemented function")
#define LIT_ERR_GET_TIME _lit_err("unable to get time")
#define LIT_ERR_TIME_UNIMPLEMENTED _lit_err("lit compiled without time support")
#define LIT_ERR_FUN_PTR _lit_err("unable to get function pointer")

/* GLenum <-> int */
#define GLenum_val(v) (GLenum)Int_val (v)
#define Val_GLenum(v) Val_int (v)

/* GLbool <-> bool */
#define GLboolean_val(v) (GLboolean)Bool_val (v)
#define Val_GLboolean(v) Val_bool (v)

/* GLint <-> int32 */
#define GLint_val(v) (GLint)Int32_val (v)
#define Val_GLint(v) copy_int32 (v)
#define GLint_var(v) GLint v 
#define GLint_ref(v) &v 

/* GLint <-> int */
#define GLint_as_int_val(v) (GLint)Int_val (v)
#define Val_GLint_as_int(v) Val_int (v)
#define GLint_as_int_var(v) GLint_var (v)
#define GLint_as_int_ref(v) GLint_ref (v)

/* GLfloat <-> float */
#define GLfloat_val(v) (GLfloat)Double_val (v)
#define Val_GLfloat(v) copy_double ((double)v)
#define GLfloat_var(v) GLfloat v
#define GLfloat_ref(v) &v

/* GLdouble <-> float */
#define GLdouble_val(v) (GLdouble)Double_val (v)
#define Val_GLdouble(v) copy_double (v)
#define GLdouble_var(v) GLdouble v
#define GLdouble_ref(v) &v

/* GLubyte* -> string */
#define Val_GLubyte_ptr(s) caml_copy_string((char *)s)

/* GLdouble* <-> float array */
#ifdef ARCH_ALIGN_DOUBLE
#error "lit cannot work for now on this platform, contact the author"
#endif
#define GLdouble_ptr_val(v) (GLdouble *)&(Double_field(v,0))
/*#define Val_GLdouble_ptr(v) */
#define GLdouble_ptr_var(v) GLdouble *v
#define GLdouble_ptr_ref(v) &v
 
/* void* <-> value */
#define Void_ptr_val(v) (void *)v
#define Val_Void_ptr(v) (value)v

/* Stub generators  */
#define lit_none(fn)				   \
  CAMLprim value lit_ ## fn (value fp)	\
  { f ## fn(fp) (); return Val_unit;  }

#define lit_in0_ret(fn,r1)				\
  CAMLprim value lit_ ## fn (value fp, value unit)	\
  { return Val_ ## r1 ( f ## fn(fp)()); }		

#define lit_in1_ret(fn,i1,r1)				\
  CAMLprim value lit_ ## fn (value fp, value v)		\
  { return Val_ ## r1 ( f ## fn(fp)(i1 ## _val (v))); }

#define lit_in2_ret(fn,i1,i2,r1)					\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2)		\
  { return Val_ ## r1 ( f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2))); }

#define lit_in1(fn,i1)						\
  CAMLprim value lit_ ## fn (value fp, value v1)		\
  { f ## fn(fp)(i1 ## _val (v1)); return Val_unit; }

#define lit_in2(fn,i1,i2)						\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2)		\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2)); return Val_unit; }

#define lit_in3(fn,i1,i2,i3)						\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3)	\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3));	\
    return Val_unit; }

#define lit_in4(fn,i1,i2,i3,i4)						\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4) \
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4));				   \
    return Val_unit; }						   \
  
#define lit_in5(fn,i1,i2,i3,i4,i5)					\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4, \
			     value v5)					\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5));			\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]); }

#define lit_in6(fn,i1,i2,i3,i4,i5,i6)	                                      \
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4,\
			     value v5, value v6)			\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6));	\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], \
		       argv[6]); }

#define lit_in7(fn,i1,i2,i3,i4,i5,i6,i7)				\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4,\
			     value v5, value v6, value v7)		\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6),	\
		i7 ## _val (v7));					\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], \
		       argv[6], argv[7]);	}

#define lit_in8(fn,i1,i2,i3,i4,i5,i6,i7,i8)	                              \
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4,\
			     value v5, value v6, value v7, value v8)	\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6),	\
		i7 ## _val (v7), i8 ## _val (v8));			\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],\
		       argv[6], argv[7], argv[8]);}

#define lit_in9(fn,i1,i2,i3,i4,i5,i6,i7,i8,i9)				\
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4, \
			     value v5, value v6, value v7, value v8, value v9) \
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6),	\
		i7 ## _val (v7), i8 ## _val (v8), i9 ## _val (v9));	\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], \
		       argv[6], argv[7], argv[8], argv[9]); }

#define lit_in10(fn,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)	                      \
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4,\
			     value v5, value v6, value v7, value v8, value v9,\
			     value v10)					\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6),	\
		i7 ## _val (v7), i8 ## _val (v8), i9 ## _val (v9),	\
		i10 ## _val (v10));					\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], \
		       argv[6], argv[7], argv[8], argv[9], argv[10]); }

#define lit_in11(fn,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)	   		      \
  CAMLprim value lit_ ## fn (value fp, value v1, value v2, value v3, value v4,\
			     value v5, value v6, value v7, value v8, value v9,\
			     value v10,	value v11)			\
  { f ## fn(fp)(i1 ## _val (v1), i2 ## _val (v2), i3 ## _val (v3),	\
		i4 ## _val (v4), i5 ## _val (v5), i6 ## _val (v6),	\
		i7 ## _val (v7), i8 ## _val (v8), i9 ## _val (v9),	\
		i10 ## _val (v10), i11 ## _val (v11));			\
    return Val_unit; }							\
  									\
  CAMLprim value lit_bc_ ## fn (value *argv, int argn)			\
  { return lit_ ## fn (argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], \
		       argv[6], argv[7], argv[8], argv[9], argv[10],	\
		       argv[11]);  }
  						

#define litf_in1_out1(litf,fn,i1,o1)				\
  CAMLprim value lit_ ## litf (value fp, value v)		\
  { o1 ## _var(t1);						\
    f ## fn(fp)(i1 ## _val (v), o1 ## _ref(t1));	        \
    return Val_ ## o1 (t1); }

#define litf_in2_out1(litf,fn,i1,i2,o1)				 \
  CAMLprim value lit_ ## litf (value fp, value v1, value v2)	 \
  { o1 ## _var(t1);						  \
    f ## fn(fp)(i1 ## _val (v1), i2 ## _val(v2), o1 ## _ref(t1)); \
    return Val_ ## o1 (t1); }

/* For glGen funcs */
#define lit_gen(fn)						    \
  CAMLprim value lit_ ## fn (value fp)				    \
  { GLuint g; f ## fn(fp)(1, &g); return copy_int32 (g); }

/* For glDelete funcfn */
#define lit_del(fn)							\
  CAMLprim value lit_ ## fn (value fp, value g)				\
  { GLuint p = Int32_val (g); f ## fn(fp)(1, &p); return Val_unit; }

#endif
