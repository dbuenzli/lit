/*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*/

#include "litstub.h"

CAMLprim value lit_debug (value *v)
{ return Val_unit; }

/* Pointers */
CAMLprim value lit_c_nil (value unit) 
{ return (value) NULL; }

CAMLprim value lit_c_offset (value c, value unit, value offset) 
{ return (value) ((void *)c + (Int_val (unit) * Int_val (offset))); }

CAMLprim value lit_c_copy (value s, value d, value unit, value offset)
{
  memcpy ((void *) d, (void *) s, Int_val (unit) * Int_val (offset));
  return Val_unit;
}

CAMLprim value lit_c_of_barray (value ba) 
{ return (value) Data_bigarray_val (ba); }

CAMLprim value lit_barray_of_c (value c, value kind, value len)
{
  long dim = Int_val (len);
  return alloc_bigarray (Int_val (kind) | BIGARRAY_C_LAYOUT | BIGARRAY_EXTERNAL,
			 1, (void *)c, &dim);
}

CAMLprim value lit_barray_invalidate (value b)
{
  struct caml_bigarray *barr = Bigarray_val (b);
  barr->data = NULL;
  barr->dim[0] = 0;
  return Val_unit;
}

CAMLprim value lit_fun_ptr (value fname)
{
  void *fptr = NULL;
  lit_glfun_addr(fptr, String_val (fname));
  if (!fptr) failwith (LIT_ERR_FUN_PTR);
  return (value) fptr;
}


/* Gl stubs */

lit_in1_ret (glGetString, GLenum, GLubyte_ptr);
litf_in1_out1 (litGetInteger, glGetIntegerv, GLenum, GLint_as_int);
lit_in0_ret (glGetError, GLenum);
lit_none (glFlush);

CAMLprim value lit_glFinish (value unit)
{ 
  caml_leave_blocking_section ();
  glFinish();
  caml_enter_blocking_section ();
  return Val_unit;
}

lit_del (glDeleteBuffersARB);
lit_gen (glGenBuffersARB); 
lit_in1 (glActiveTexture, GLenum);
lit_in1 (glClear, GLint_as_int);
lit_in1 (glClearDepth, GLfloat);
lit_in1 (glClearStencil, GLint);
lit_in1 (glCullFace, GLenum);
lit_in1 (glDepthFunc, GLenum);
lit_in1 (glDepthMask, GLboolean);
lit_in1 (glDisable, GLenum);
lit_in1 (glDisableClientState, GLenum);
lit_in1 (glEnable, GLenum);
lit_in1 (glEnableClientState, GLenum);
lit_in1 (glLoadMatrixd, GLdouble_ptr); /* TODO remove */
lit_in1 (glMatrixMode, GLenum); /* TODO remove */
lit_in1 (glMultMatrixd, GLdouble_ptr); /* TODO remove */
lit_in1 (glShadeModel, GLenum);
lit_in1 (glStencilMask, GLint);
lit_in2 (glAlphaFunc, GLenum, GLfloat);
lit_in2 (glBindBufferARB, GLenum, GLint);
lit_in2 (glBlendFunc, GLenum, GLenum);
lit_in2 (glDepthRange, GLdouble, GLdouble);
lit_in2 (glPolygonMode, GLenum, GLenum);
lit_in2 (glPolygonOffset, GLfloat, GLfloat);
lit_in3 (glStencilFunc, GLenum, GLint, GLint);
lit_in3 (glStencilOp, GLenum, GLenum, GLenum);
lit_in4 (glClearAccum, GLfloat, GLfloat, GLfloat, GLfloat);
lit_in4 (glClearColor, GLfloat, GLfloat, GLfloat, GLfloat);
lit_in4 (glColorMask, GLboolean, GLboolean, GLboolean, GLboolean);
lit_in4 (glScissor, GLint_as_int, GLint_as_int, GLint_as_int, GLint_as_int);
lit_in4 (glViewport, GLint_as_int, GLint_as_int, GLint_as_int, GLint_as_int);

/*
lit_in1 (glEnableVertexAttribArray, GLint_as_int);   TODO should be GLuint */
/* 
lit_in1 (glDisableVertexAttribArray, GLint_as_int);  TODO should be GLuint */

CAMLprim value lit_glBufferDataARB (value fp, value t, value unit, value size, 
				    value c, value u)
{
  caml_leave_blocking_section ();
  fglBufferDataARB(fp) (GLenum_val (t), Int_val (unit) * Int_val (size), 
		       Void_ptr_val (c), GLenum_val (u));
  caml_enter_blocking_section ();
  return Val_unit;
}

CAMLprim value lit_bc_glBufferDataARB (value *argv, int argn)
{
  lit_glBufferDataARB(argv[0], argv[1], argv[2], argv[3], argv[4],
		      argv[5]);
  return Val_unit;
}

CAMLprim value lit_glBufferSubDataARB (value fp, value t, value unit, 
				       value offset, value size, value c)
{
  caml_leave_blocking_section ();
  fglBufferSubDataARB(fp) (GLenum_val (t), Int_val (unit) * Int_val (offset), 
			    Int_val (unit) * Int_val (size), Void_ptr_val (c)); 
  caml_enter_blocking_section ();
  return Val_unit;
}

CAMLprim value lit_bc_glBufferSubDataARB (value *argv, int argn)
{
  lit_glBufferSubDataARB(argv[0], argv[1], argv[2], argv[3], argv[4],
			 argv[5]);
  return Val_unit;
}


CAMLprim value lit_glGetBufferSubDataARB (value fp, value t, value unit, 
					  value offset, value size, value c)
{
  caml_leave_blocking_section ();
  fglGetBufferSubDataARB(fp) (GLenum_val (t), 
			      Int_val (unit) * Int_val (offset), 
			      Int_val (unit) * Int_val (offset), 
			      Void_ptr_val (c)); 
  caml_enter_blocking_section ();
  return Val_unit;
}

CAMLprim value lit_bc_glGetBufferSubDataARB (value *argv, int argn)
{
  lit_glGetBufferSubDataARB(argv[0], argv[1], argv[2], argv[3], argv[4],
			    argv[5]);
  return Val_unit;
}


CAMLprim value lit_glMapBufferARB (value fp, value t, value a)
{
  void *c;
  caml_leave_blocking_section ();
  c =  fglMapBufferARB(fp) (GLenum_val (t), GLenum_val (a));
  caml_enter_blocking_section ();
  return Val_Void_ptr (c);
}

lit_in1_ret (glUnmapBufferARB, GLenum, GLboolean);
lit_in2 (glPixelStorei, GLenum, GLint_as_int);
lit_in7 (glReadPixels, GLint_as_int, GLint_as_int, GLint_as_int, GLint_as_int,
	 GLenum, GLenum, Void_ptr);

lit_gen (glGenTextures);
lit_del (glDeleteTextures);
lit_in2 (glBindTexture, GLenum, GLint);
lit_in3 (glTexParameteri, GLenum, GLenum, GLint_as_int);
litf_in2_out1 (litGetTexParameteri, glGetTexParameteriv, GLenum, GLenum, 
	       GLint_as_int);
lit_in3 (glTexParameterf, GLenum, GLenum, GLfloat);
litf_in2_out1 (litGetTexParameterf, glGetTexParameterfv, GLenum, GLenum, 
	       GLfloat);
lit_in8 (glTexImage1D, GLenum, GLint_as_int, GLenum, GLint_as_int, GLint_as_int,
	 GLenum, GLenum, Void_ptr);
lit_in9 (glTexImage2D, GLenum, GLint_as_int, GLenum, GLint_as_int, GLint_as_int,
	 GLint_as_int, GLenum, GLenum, Void_ptr);
lit_in10 (glTexImage3D, GLenum, GLint_as_int, GLenum, GLint_as_int, 
	  GLint_as_int, GLint_as_int, GLint_as_int, GLenum, GLenum, Void_ptr);
lit_in7 (glTexSubImage1D, GLenum, GLint_as_int, GLint_as_int, GLint_as_int,
	 GLenum, GLenum, Void_ptr);
lit_in9 (glTexSubImage2D, GLenum, GLint_as_int, GLint_as_int, GLint_as_int,
	 GLint_as_int, GLint_as_int, GLenum, GLenum, Void_ptr);
lit_in11 (glTexSubImage3D, GLenum, GLint_as_int, GLint_as_int, GLint_as_int,
	  GLint_as_int, GLint_as_int, GLint_as_int, GLint_as_int, GLenum,
	  GLenum, Void_ptr);
lit_in4 (glVertexPointer, GLint_as_int, GLenum, GLint_as_int, Void_ptr);
lit_in4 (glColorPointer, GLint_as_int, GLenum, GLint_as_int, Void_ptr);
lit_in3 (glNormalPointer, GLenum, GLint_as_int, Void_ptr);
lit_in4 (glTexCoordPointer, GLint_as_int, GLenum, GLint_as_int, Void_ptr);
lit_in3 (glDrawArrays, GLenum, GLint_as_int, GLint_as_int);
lit_in4 (glDrawElements, GLenum, GLint_as_int, GLenum, Void_ptr);
lit_in6 (glDrawRangeElements, GLenum, GLint_as_int, GLint_as_int, 
	 GLint_as_int, GLenum, Void_ptr);

CAMLprim value lit_draw_tex_rect(value v2)
{
  static float v[8] = { 0.0f, 0.0f,   1.0f, 0.0f,  1.0f, 1.0f,  0.0f, 1.0f };
  static float t[8] = { 0.0f, 0.0f,   1.0f, 0.0f,  1.0f, 1.0f,  0.0f, 1.0f };
  t[2] = t[4] = (float) Double_field (v2, 0);
  t[5] = t[7] = (float) Double_field (v2, 1);

  glBindBuffer (GL_ARRAY_BUFFER_ARB, 0);
  glEnableClientState (GL_VERTEX_ARRAY);
  glEnableClientState (GL_TEXTURE_COORD_ARRAY);
  glTexCoordPointer (2, GL_FLOAT, 0, &t);
  glVertexPointer (2, GL_FLOAT, 0, &v);
  glDrawArrays (GL_QUADS, 0, 4);
  glDisableClientState (GL_VERTEX_ARRAY);
  glDisableClientState (GL_TEXTURE_COORD_ARRAY);
  return Val_unit;
}


/* Time stubs */
#ifdef LIT_WITH_TIME
#include <sys/time.h>
CAMLprim value lit_time_now (value unit) 
{
  struct timeval t;
  if (gettimeofday (&t, NULL) == -1) failwith (LIT_ERR_GET_TIME);
  return copy_double ((double) t.tv_sec + (double) t.tv_usec * 1e-6);
}
#else
CAMLprim value lit_time_now (value unit) 
{ failwith (LIT_ERR_TIME_UNIMPLEMENTED); }
#endif
