/*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*/

#ifdef LIT_MACOSX
#include <OpenGL/gl.h>
#define lit_glfun_addr(fptr,fname)\
  NSSymbol s = NULL;							\
  char name[255] = "_";							\
  strcpy(name + 1, fname);						\
  if (NSIsSymbolNameDefined (name)) s = NSLookupAndBindSymbol (name);	\
  fptr = s ? NSAddressOfSymbol (s) : NULL

#elif LIT_X11
#include <GL/gl.h>
#include <GL/glx.h>
#define lit_glfun_addr(fptr,fname)					\
  fptr = glXGetProcAddressARB ((GLubyte *) fname)

#elif LIT_MSWIN
#include <GL/gl.h>
#include <GL/wgl.h>
#define lit_glfun_addr(fptr,fname)					\
  fptr = wglGetProcAddress ((GLubyte *) fname) 

#elif
#error "A platform must be specified"
#endif
#ifndef GL_VERSION_1_5
#error "The header gl.h with versio 1.5 is needed to compile lit"
#endif

/* OpenGL function types */

#define fglActiveTexture(f) (*(void (*)(GLenum texture))f)
#define fglAlphaFunc(f) (*(void (*)(GLenum func, GLclampf ref))f)
#define fglBindBufferARB(f) (*(void (*)(GLenum target, GLuint buffer))f)
#define fglBindTexture(f) (*(void (*)(GLenum target, GLuint texture))f)
#define fglBlendFunc(f) (*(void (*)(GLenum sfactor, GLenum dfactor))f)
#define fglBufferDataARB(f) ((*(void (*)(GLenum target, GLsizeiptr size, const GLvoid *data, GLenum usage))f))
#define fglBufferSubDataARB(f) (*(void (*)(GLenum target, GLintptr offset, GLsizeiptr size, const GLvoid *data))f)
#define fglClear(f) (*(void (*)(GLbitfield mask))f)
#define fglClearAccum(f) (*(void (*)(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha))f)
#define fglClearColor(f) (*(void (*)(GLclampf red, GLclampf green, GLclampf blue, GLclampf alpha))f)
#define fglClearDepth(f) (*(void (*)(GLclampd depth))f)
#define fglClearStencil(f) (*(void (*)(GLint s))f)
#define fglColorMask(f) (*(void (*) (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha))f)
#define fglColorPointer(f) (*(void (*)(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer))f) /* TODO remove */ 
#define fglCullFace(f) (*(void (*)(GLenum mode))f)
#define fglDeleteBuffersARB(f) (*(void (*)(GLsizei n, const GLuint *buffers))f)
#define fglDeleteTextures(f) (*(void (*) (GLsizei n, const GLuint *textures))f)
#define fglDepthFunc(f) (*(void (*)(GLenum func))f)
#define fglDepthMask(f) (*(void (*)(GLboolean flag))f)
#define fglDepthRange(f) (*(void (*)(GLclampd zNear, GLclampd zFar))f)
#define fglDisable(f) (*(void (*)(GLenum cap))f)
#define fglDisableClientState(f) (*(void (*)(GLenum array))f)
#define fglDrawArrays(f) (*(void (*)(GLenum mode, GLint first, GLsizei count))f)
#define fglDrawElements(f) (*(void (*)(GLenum mode, GLsizei count, GLenum type, const GLvoid *indices))f)
#define fglDrawRangeElements(f) (*(void (*)(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const GLvoid *indices))f)
#define fglEnable(f) (*(void (*)(GLenum cap))f)
#define fglEnableClientState(f) (*(void (*)(GLenum array))f)
#define fglFinish(f) (*(void (*)(void))f)
#define fglFlush(f) (*(void (*)(void))f)
#define fglGenBuffersARB(f) (*(void (*)(GLsizei n, GLuint *buffers))f)
#define fglGenTextures(f)  (*(void (*) (GLsizei n, GLuint *textures))f)
#define fglGetBufferSubDataARB(f) (*(void (*)(GLenum target, GLintptr offset, GLsizeiptr size, GLvoid *data))f)
#define fglGetError(f) (*(GLenum (*)(void))f)
#define fglGetIntegerv(f) (*(void (*)(GLenum pname, GLint *params))f)
#define fglGetString(f) (*(GLubyte * (*)(GLenum name))f)
#define fglGetTexParameterfv(f) (*(void (*) (GLenum target, GLenum pname, GLfloat *params))f)
#define fglGetTexParameteriv(f) (*(void (*) (GLenum target, GLenum pname, GLint *params))f)
#define fglLoadMatrixd(f) (*(void (*)(const GLdouble *m))f) /* TODO remove */ 
#define fglMapBufferARB(f) (*(GLvoid * (*)(GLenum target, GLenum access))f)
#define fglMatrixMode(f) (*(void (*)(GLenum mode))f) /* TODO remove */ 
#define fglMultMatrixd(f) (*(void (*)(const GLdouble *m))f) /* TODO remove */ 
#define fglNormalPointer(f) (*(void (*)(GLenum type, GLsizei stride, const GLvoid *pointer))f) /* TODO remove */ 
#define fglPixelStorei(f) (*(void (*)(GLenum pname, GLint param))f)
#define fglPolygonMode(f) (*(void (*)(GLenum face, GLenum mode))f)
#define fglPolygonOffset(f) (*(void (*)(GLfloat factor, GLfloat units))f)
#define fglReadPixels(f) (*(void (*)(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels))f)
#define fglScissor(f) (*(void (*)(GLint x, GLint y, GLsizei width, GLsizei height))f)
#define fglShadeModel(f) (*(void (*)(GLenum mode))f)
#define fglStencilFunc(f) (*(void (*)(GLenum func, GLint ref, GLuint mask))f)
#define fglStencilMask(f) (*(void (*)(GLuint mask))f)
#define fglStencilOp(f) (*(void (*)(GLenum fail, GLenum zfail, GLenum zpass))f)
#define fglTexCoordPointer(f) (*(void (*)(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer))f) /* TODO remove */ 
#define fglTexImage1D(f) (*(void (*)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglTexImage2D(f) (*(void (*)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglTexImage3D(f) (*(void (*)(GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglTexParameterf(f) (*(void (*)(GLenum target, GLenum pname, GLfloat param))f)
#define fglTexParameterfv(f) (*(void (*)(GLenum target, GLenum pname, const GLfloat *params))f)
#define fglTexParameteri(f) (*(void (*)(GLenum target, GLenum pname, GLint param))f)
#define fglTexSubImage1D(f) (*(void (*)(GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglTexSubImage2D(f) (*(void (*)(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglTexSubImage3D(f) (*(void (*)(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const GLvoid *pixels))f)
#define fglUnmapBufferARB(f) (*(GLboolean (*)(GLenum target))f)
#define fglVertexPointer(f) (*(void (*)(GLint size, GLenum type, GLsizei stride, const GLvoid *pointer))f) /* TODO remove */ 
#define fglViewport(f) (*(void (*)(GLint x, GLint y, GLsizei width, GLsizei height))f)




/* OpenGL enumerants */

#ifndef GL_ARB_texture_float
#define GL_TEXTURE_RED_TYPE_ARB           0x8C10
#define GL_TEXTURE_GREEN_TYPE_ARB         0x8C11
#define GL_TEXTURE_BLUE_TYPE_ARB          0x8C12
#define GL_TEXTURE_ALPHA_TYPE_ARB         0x8C13
#define GL_TEXTURE_LUMINANCE_TYPE_ARB     0x8C14
#define GL_TEXTURE_INTENSITY_TYPE_ARB     0x8C15
#define GL_TEXTURE_DEPTH_TYPE_ARB         0x8C16
#define GL_UNSIGNED_NORMALIZED_ARB        0x8C17
#define GL_RGBA32F_ARB                    0x8814
#define GL_RGB32F_ARB                     0x8815
#define GL_ALPHA32F_ARB                   0x8816
#define GL_INTENSITY32F_ARB               0x8817
#define GL_LUMINANCE32F_ARB               0x8818
#define GL_LUMINANCE_ALPHA32F_ARB         0x8819
#define GL_RGBA16F_ARB                    0x881A
#define GL_RGB16F_ARB                     0x881B
#define GL_ALPHA16F_ARB                   0x881C
#define GL_INTENSITY16F_ARB               0x881D
#define GL_LUMINANCE16F_ARB               0x881E
#define GL_LUMINANCE_ALPHA16F_ARB         0x881F
#endif


#ifndef GL_ARB_vertex_buffer_object
#define GL_ARRAY_BUFFER_ARB               0x8892
#define GL_ELEMENT_ARRAY_BUFFER_ARB       0x8893
#define GL_READ_ONLY_ARB                  0x88B8
#define GL_WRITE_ONLY_ARB                 0x88B9
#define GL_READ_WRITE_ARB                 0x88BA
#define GL_BUFFER_ACCESS_ARB              0x88BB
#define GL_STREAM_DRAW_ARB                0x88E0
#define GL_STREAM_READ_ARB                0x88E1
#define GL_STREAM_COPY_ARB                0x88E2
#define GL_STATIC_DRAW_ARB                0x88E4
#define GL_STATIC_READ_ARB                0x88E5
#define GL_STATIC_COPY_ARB                0x88E6
#define GL_DYNAMIC_DRAW_ARB               0x88E8
#define GL_DYNAMIC_READ_ARB               0x88E9
#define GL_DYNAMIC_COPY_ARB               0x88EA
#endif

#ifndef GL_ARB_pixel_buffer_object
#define GL_PIXEL_PACK_BUFFER_ARB          0x88EB
#define GL_PIXEL_UNPACK_BUFFER_ARB        0x88EC
#endif

#ifndef GL_ARB_shading_language_100
#define GL_SHADING_LANGUAGE_VERSION_ARB   0x8B8C
#endif

#ifdef GL_SHADING_LANGUAGE_VERSION
#undef GL_SHADING_LANGUAGE_VERSION_ARB
#define GL_SHADING_LANGUAGE_VERSION_ARB GL_SHADING_LANGUAGE_VERSION
#endif


