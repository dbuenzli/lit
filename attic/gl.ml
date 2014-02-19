(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

let str s = Printf.sprintf s 
let err_fun_ptr s = failwith (str "unable to get function pointer for %s" s)

module Enum = struct
  type astorage = int
  type blend_factor = int
  type buf_access = int
  type buf_target = int
  type buf_usage = int
  type buffer_bit_field = int
  type cap = int
  type client_cap = int
  type err = int
  type face = int
  type func = int
  type get_int = int
  type get_str = int
  type matrix_mode = int (* TODO remove *) 
  type mode = int
  type pformat = int
  type pixel_store_param = int
  type polygon_mode = int
  type pstorage = int
  type shade_model = int
  type stencil_op = int
  type tex_paramf = int
  type tex_parami = int
  type tex_pformat = int
  type tex_target = int
  type tex_unit = int
end

(* Enumerant constants *)
let abyte = GL_BYTE
let accum_alpha_bits = GL_ACCUM_ALPHA_BITS
let accum_blue_bits = GL_ACCUM_BLUE_BITS
let accum_buffer_bit = GL_ACCUM_BUFFER_BIT
let accum_green_bits = GL_ACCUM_GREEN_BITS
let accum_red_bits = GL_ACCUM_RED_BITS
let adouble = GL_DOUBLE
let afloat = GL_FLOAT
let aint = GL_INT
let alpha = GL_ALPHA
let alpha12 = GL_ALPHA12
let alpha16 = GL_ALPHA12
let alpha16f = GL_ALPHA16F_ARB
let alpha32f = GL_ALPHA32F_ARB
let alpha4 = GL_ALPHA4
let alpha8 = GL_ALPHA8
let alpha_bits = GL_ALPHA_BITS
let alpha_test = GL_ALPHA_TEST
let always = GL_ALWAYS
let array_buffer = GL_ARRAY_BUFFER_ARB
let ashort = GL_SHORT
let aunsigned_byte = GL_UNSIGNED_BYTE
let aunsigned_int = GL_UNSIGNED_INT
let aunsigned_short = GL_UNSIGNED_SHORT
let back = GL_BACK
let bgr = GL_BGR
let bgra = GL_BGRA
let blend = GL_BLEND
let blend_factor_one = GL_ONE
let blend_factor_zero = GL_ZERO
let blue = GL_BLUE
let blue_bits = GL_BLUE_BITS
let cap_cull_face = GL_CULL_FACE
let clamp = GL_CLAMP
let clamp_to_border = GL_CLAMP_TO_BORDER
let clamp_to_edge = GL_CLAMP_TO_EDGE
let color_array = GL_COLOR_ARRAY (* TODO remove *)
let color_buffer_bit = GL_COLOR_BUFFER_BIT
let color_index = GL_COLOR_INDEX
let compressed_rgb_s3tc_dxt1 = GL_COMPRESSED_RGB_S3TC_DXT1_EXT
let compressed_rgba_s3tc_dxt1 = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
let compressed_rgba_s3tc_dxt3 = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
let compressed_rgba_s3tc_dxt5 = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
let decr = GL_DECR
let depth_bits = GL_DEPTH_BITS
let depth_buffer_bit = GL_DEPTH_BUFFER_BIT
let depth_component = GL_DEPTH_COMPONENT
let depth_component16 = GL_DEPTH_COMPONENT16
let depth_component24 = GL_DEPTH_COMPONENT24
let depth_component32 = GL_DEPTH_COMPONENT32
let depth_test = GL_DEPTH_TEST
let depth_texture_mode = GL_DEPTH_TEXTURE_MODE
let dst_alpha = GL_DST_ALPHA
let dst_color = GL_DST_COLOR
let dynamic_copy = GL_DYNAMIC_COPY_ARB
let dynamic_draw = GL_DYNAMIC_DRAW_ARB
let dynamic_read = GL_DYNAMIC_READ_ARB
let element_array_buffer = GL_ELEMENT_ARRAY_BUFFER_ARB
let equal = GL_EQUAL
let extensions = GL_EXTENSIONS
let fill = GL_FILL
let flat = GL_FLAT
let front = GL_FRONT
let front_and_back = GL_FRONT_AND_BACK
let generate_mipmap = GL_GENERATE_MIPMAP
let gequal = GL_GEQUAL
let greater = GL_GREATER
let green = GL_GREEN
let green_bits = GL_GREEN_BITS
let incr = GL_INCR
let intensity12 = GL_INTENSITY12
let intensity16 = GL_INTENSITY16
let intensity16f = GL_INTENSITY16F_ARB
let intensity32f = GL_INTENSITY32F_ARB
let intensity4 = GL_INTENSITY4
let intensity8 = GL_INTENSITY8
let invalid_enum = GL_INVALID_ENUM
let invalid_operation = GL_INVALID_OPERATION
let invalid_value = GL_INVALID_VALUE
let invert = GL_INVERT
let keep = GL_KEEP
let lequal = GL_LEQUAL
let less = GL_LESS
let line = GL_LINE
let line_loop = GL_LINE_LOOP
let line_strip = GL_LINE_STRIP
let linear = GL_LINEAR
let linear_mipmap_linear = GL_LINEAR_MIPMAP_LINEAR
let linear_mipmap_nearest = GL_LINEAR_MIPMAP_NEAREST
let lines = GL_LINES
let lit_zero_buffer_bit = 0
let luminance = GL_LUMINANCE
let luminance12 = GL_LUMINANCE12
let luminance12_alpha12 = GL_LUMINANCE12_ALPHA12
let luminance16 = GL_LUMINANCE16
let luminance16_alpha16 = GL_LUMINANCE16_ALPHA16
let luminance16f = GL_LUMINANCE16F_ARB
let luminance32f = GL_LUMINANCE32F_ARB
let luminance4 = GL_LUMINANCE4
let luminance4_alpha4 = GL_LUMINANCE4_ALPHA4
let luminance6_alpha6 = GL_LUMINANCE6_ALPHA2
let luminance8 = GL_LUMINANCE8
let luminance8_alpha8 = GL_LUMINANCE8_ALPHA8
let luminance_alpha = GL_LUMINANCE_ALPHA
let luminance_alpha16f = GL_LUMINANCE_ALPHA16F_ARB 
let luminance_alpha32f = GL_LUMINANCE_ALPHA32F_ARB
let max_3d_texture_size = GL_MAX_3D_TEXTURE_SIZE
let max_clip_planes = GL_MAX_CLIP_PLANES
let max_combined_texture_image_units = GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB
let max_elements_indices = GL_MAX_ELEMENTS_INDICES
let max_elements_vertices = GL_MAX_ELEMENTS_VERTICES
let max_fragment_texture_image_units = GL_MAX_TEXTURE_IMAGE_UNITS_ARB
let max_fragment_uniform_components = GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB
let max_texture_size = GL_MAX_TEXTURE_SIZE
let max_varying_floats = GL_MAX_VARYING_FLOATS_ARB
let max_vertex_attribs = GL_MAX_VERTEX_ATTRIBS_ARB
let max_vertex_texture_image_units = GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB
let max_vertex_uniform_components = GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB
let mirrored_repeat = GL_MIRRORED_REPEAT
let modelview = GL_MODELVIEW
let nearest = GL_NEAREST
let nearest_mipmap_linear = GL_NEAREST_MIPMAP_LINEAR
let nearest_mipmap_nearest = GL_NEAREST_MIPMAP_NEAREST
let never = GL_NEVER
let no_error = GL_NO_ERROR
let normal_array = GL_NORMAL_ARRAY (* TODO remove *)
let notequal = GL_NOTEQUAL
let one_minus_dst_alpha = GL_ONE_MINUS_DST_ALPHA
let one_minus_dst_color = GL_ONE_MINUS_DST_COLOR
let one_minus_src_alpha = GL_ONE_MINUS_SRC_ALPHA
let one_minus_src_color = GL_ONE_MINUS_SRC_COLOR
let out_of_memory = GL_OUT_OF_MEMORY
let pack_alignment = GL_PACK_ALIGNMENT
let pack_image_height = GL_PACK_IMAGE_HEIGHT
let pack_lsb_first = GL_PACK_LSB_FIRST
let pack_row_length = GL_PACK_ROW_LENGTH
let pack_skip_images = GL_PACK_SKIP_IMAGES
let pack_skip_pixels = GL_PACK_SKIP_PIXELS
let pack_skip_rows = GL_PACK_SKIP_ROWS
let pack_swap_bytes = GL_PACK_SWAP_BYTES
let pbitmap = GL_BITMAP
let pbyte = GL_BYTE
let pfloat = GL_FLOAT
let pint = GL_INT
let pixel_pack_buffer = GL_PIXEL_PACK_BUFFER_ARB
let pixel_unpack_buffer = GL_PIXEL_UNPACK_BUFFER_ARB
let point = GL_POINT
let points = GL_POINTS
let polygon = GL_POLYGON
let polygon_offset_fill = GL_POLYGON_OFFSET_FILL
let polygon_offset_line = GL_POLYGON_OFFSET_LINE
let polygon_offset_point = GL_POLYGON_OFFSET_POINT
let projection = GL_PROJECTION
let pshort = GL_SHORT
let punsigned_byte = GL_UNSIGNED_BYTE
let punsigned_byte_2_3_3_rev = GL_UNSIGNED_BYTE_2_3_3_REV
let punsigned_byte_3_3_2 = GL_UNSIGNED_BYTE_3_3_2
let punsigned_int = GL_UNSIGNED_INT
let punsigned_int_10_10_10_2 = GL_UNSIGNED_INT_10_10_10_2
let punsigned_int_2_10_10_10_rev = GL_UNSIGNED_INT_2_10_10_10_REV
let punsigned_int_8_8_8_8 = GL_UNSIGNED_INT_8_8_8_8
let punsigned_int_8_8_8_8_rev = GL_UNSIGNED_INT_8_8_8_8_REV
let punsigned_short = GL_UNSIGNED_SHORT
let punsigned_short_1_5_5_5_rev = GL_UNSIGNED_SHORT_1_5_5_5_REV
let punsigned_short_4_4_4_4 = GL_UNSIGNED_SHORT_4_4_4_4
let punsigned_short_4_4_4_4_rev = GL_UNSIGNED_SHORT_4_4_4_4_REV
let punsigned_short_5_5_5_1 = GL_UNSIGNED_SHORT_5_5_5_1
let punsigned_short_5_6_5 = GL_UNSIGNED_SHORT_5_6_5
let punsigned_short_5_6_5_rev = GL_UNSIGNED_SHORT_5_6_5_REV
let quad_strip = GL_QUAD_STRIP
let quads = GL_QUADS
let r3_g3_b2 = GL_R3_G3_B2 
let read_only = GL_READ_ONLY_ARB
let read_write = GL_READ_WRITE_ARB
let red = GL_RED
let red_bits = GL_RED_BITS
let renderer = GL_RENDERER
let repeat = GL_REPEAT
let replace = GL_REPLACE
let rgb = GL_RGB
let rgb10 = GL_RGB10
let rgb10_a2 = GL_RGB10_A2
let rgb12 = GL_RGB12
let rgb16 = GL_RGB16
let rgb16f = GL_RGB16F_ARB
let rgb32f = GL_RGB32F_ARB
let rgb4 = GL_RGB4
let rgb5 = GL_RGB5
let rgb5_a1 = GL_RGB5_A1
let rgb8 = GL_RGB8
let rgba = GL_RGBA
let rgba12 = GL_RGBA12
let rgba16 = GL_RGBA12
let rgba16f = GL_RGBA16F_ARB
let rgba2 = GL_RGBA2
let rgba32f = GL_RGBA32F_ARB
let rgba4 = GL_RGBA4
let rgba8 = GL_RGBA8
let scissor_test = GL_SCISSOR_TEST
let smooth = GL_SMOOTH
let src_alpha = GL_SRC_ALPHA
let src_alpha_saturate = GL_SRC_ALPHA_SATURATE
let src_color = GL_SRC_COLOR
let stack_overflow = GL_STACK_OVERFLOW
let stack_underflow = GL_STACK_UNDERFLOW
let static_copy = GL_STATIC_COPY_ARB
let static_draw = GL_STATIC_DRAW_ARB
let static_read = GL_STATIC_READ_ARB
let stencil_bits = GL_STENCIL_BITS
let stencil_buffer_bit = GL_STENCIL_BUFFER_BIT
let stencil_index = GL_STENCIL_INDEX
let stencil_test = GL_STENCIL_TEST
let stream_copy = GL_STREAM_COPY_ARB
let stream_draw = GL_STREAM_DRAW_ARB
let stream_read = GL_STREAM_READ_ARB
let table_too_large = GL_TABLE_TOO_LARGE
let texture = GL_TEXTURE
let texture_1D = GL_TEXTURE_1D
let texture_2D = GL_TEXTURE_2D
let texture_3D = GL_TEXTURE_3D
let texture_base_level = GL_TEXTURE_BASE_LEVEL
let texture_compare_func = GL_TEXTURE_COMPARE_FUNC
let texture_compare_mode = GL_TEXTURE_COMPARE_MODE
let texture_coord_array = GL_TEXTURE_COORD_ARRAY (* TODO remove *)
let texture_cube_map = GL_TEXTURE_CUBE_MAP
let texture_lod_bias = GL_TEXTURE_LOD_BIAS
let texture_mag_filter = GL_TEXTURE_MAG_FILTER
let texture_max_level = GL_TEXTURE_MAX_LEVEL
let texture_max_lod = GL_TEXTURE_MAX_LOD
let texture_min_filter = GL_TEXTURE_MIN_FILTER
let texture_min_lod = GL_TEXTURE_MIN_LOD
let texture_priority = GL_TEXTURE_PRIORITY
let texture_rectangle = GL_TEXTURE_RECTANGLE_ARB
let texture_resident = GL_TEXTURE_RESIDENT    (** only for getting *)
let texture_wrap_r = GL_TEXTURE_WRAP_R
let texture_wrap_s = GL_TEXTURE_WRAP_S
let texture_wrap_t = GL_TEXTURE_WRAP_T
let triangle_fan = GL_TRIANGLE_FAN
let triangle_strip = GL_TRIANGLE_STRIP
let triangles = GL_TRIANGLES
let ttexture_1D = GL_TEXTURE_1D
let ttexture_2D = GL_TEXTURE_2D
let ttexture_3D = GL_TEXTURE_3D
let ttexture_cube_map = GL_TEXTURE_CUBE_MAP
let ttexture_cube_map_negative_x = GL_TEXTURE_CUBE_MAP_NEGATIVE_X
let ttexture_cube_map_negative_y = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
let ttexture_cube_map_negative_z = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
let ttexture_cube_map_positive_x = GL_TEXTURE_CUBE_MAP_POSITIVE_X
let ttexture_cube_map_positive_y = GL_TEXTURE_CUBE_MAP_POSITIVE_Y
let ttexture_cube_map_positive_z = GL_TEXTURE_CUBE_MAP_POSITIVE_Z
let ttexture_rectangle = GL_TEXTURE_RECTANGLE_ARB
let unpack_alignment = GL_UNPACK_ALIGNMENT
let unpack_image_height = GL_UNPACK_IMAGE_HEIGHT
let unpack_lsb_first = GL_UNPACK_LSB_FIRST
let unpack_row_length = GL_UNPACK_ROW_LENGTH
let unpack_skip_images = GL_UNPACK_SKIP_IMAGES
let unpack_skip_pixels = GL_UNPACK_SKIP_PIXELS
let unpack_skip_rows = GL_UNPACK_SKIP_ROWS
let unpack_swap_bytes = GL_UNPACK_SWAP_BYTES
let vendor = GL_VENDOR
let version = GL_VERSION
let shading_language_version = GL_SHADING_LANGUAGE_VERSION_ARB
let vertex_array = GL_VERTEX_ARRAY (* TODO remove *)
let write_only = GL_WRITE_ONLY_ARB
let zero = GL_ZERO

module Ptr = struct
  type g = int32
  let g_nil = 0l
  type c
  external c_nil : unit -> c = "lit_c_nil"
  let c_nil = c_nil ()
  external c_offset : c -> unit:int -> offset:int -> c = "lit_c_offset"
  external c_copy : src:c -> dst:c -> unit:int -> len:int -> unit= "lit_c_copy"

  type ('a, 'b) barray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  external c_of_barray : ('a, 'b) barray -> c = "lit_c_of_barray"
  external barray_of_c : c -> ('a, 'b) Bigarray.kind -> len:int ->
    ('a, 'b) barray =  "lit_barray_of_c"
  external barray_invalidate : ('a, 'b) barray -> unit = "lit_barray_invalidate"

  module Cache = struct
    type t = { 
	mutable active_texture : int;
	mutable matrix_mode : Enum.matrix_mode }
	  
    let create () = { active_texture = 0; matrix_mode = modelview; }
  end
      
  type f
  type ft = {
      glActiveTexture : f;
      glAlphaFunc : f;
      glBindBufferARB : f;
      glBindTexture : f;
      glBlendFunc : f;
      glBufferDataARB : f;
      glBufferSubDataARB : f;
      glClear : f;
      glClearAccum : f;
      glClearColor : f;
      glClearDepth : f;
      glClearStencil : f;
      glColorMask : f;
      glColorPointer : f; (* TODO remove *) 
      glCullFace : f;
      glDeleteBuffersARB : f;
      glDeleteTextures : f;
      glDepthFunc : f;
      glDepthRange : f;
      glDepthMask : f;
      glDisable : f;
      glDisableClientState : f;
      glDrawArrays : f;
      glDrawElements : f;
      glDrawRangeElements : f;
      glEnable : f;
      glEnableClientState : f;
      glFinish : f;
      glFlush : f;
      glGenBuffersARB : f;
      glGenTextures : f;
      glGetBufferSubDataARB : f;
      glGetError : f;
      glGetIntegerv : f;
      glGetTexParameterfv : f;
      glGetTexParameteriv : f;
      glLoadMatrixd : f; (* TODO remove *) 
      glMapBufferARB : f;
      glMatrixMode : f; (* TODO remove *) 
      glMultMatrixd : f; (* TODO remove *) 
      glNormalPointer : f; (* TODO remove *) 
      glPixelStorei : f;
      glPolygonMode : f;
      glPolygonOffset : f;
      glReadPixels : f;
      glScissor : f;
      glShadeModel : f;
      glStencilFunc : f;
      glStencilMask : f;
      glStencilOp : f;
      glTexCoordPointer : f; (* TODO remove *) 
      glTexImage1D : f;
      glTexImage2D : f;
      glTexImage3D : f;
      glTexParameterf : f;
      glTexParameterfv : f;            
      glTexParameteri : f;
      glTexParameteriv : f;
      glTexSubImage1D : f;
      glTexSubImage2D : f;
      glTexSubImage3D : f;
      glUnmapBufferARB : f;
      glVertexPointer : f; (* TODO remove *) 
      glViewport : f;
      c : Cache.t }

  external _fun_ptr : string -> f = "lit_fun_ptr"
  let fun_ptr s = try _fun_ptr s with Failure _ -> err_fun_ptr s
 
  let fun_table () = 
    { glActiveTexture = fun_ptr "glActiveTexture";
      glAlphaFunc = fun_ptr "glAlphaFunc";
      glBindBufferARB = fun_ptr "glBindBufferARB";
      glBindTexture = fun_ptr "glBindTexture";
      glBlendFunc = fun_ptr "glBlendFunc";
      glBufferDataARB = fun_ptr "glBufferDataARB";
      glBufferSubDataARB = fun_ptr "glBufferSubDataARB";
      glClear = fun_ptr "glClear";
      glClearAccum = fun_ptr "glClearAccum";
      glClearColor = fun_ptr "glClearColor";
      glClearDepth = fun_ptr "glClearDepth";
      glClearStencil = fun_ptr "glClearStencil";
      glColorMask = fun_ptr "glColorMask";
      glColorPointer = fun_ptr "glColorPointer";
      glCullFace = fun_ptr "glCullFace";
      glDeleteBuffersARB = fun_ptr "glDeleteBuffersARB";
      glDeleteTextures = fun_ptr "glDeleteTextures";
      glDepthFunc = fun_ptr "glDepthFunc";
      glDepthRange = fun_ptr "glDepthRange";
      glDepthMask = fun_ptr "glDepthMask";
      glDisable = fun_ptr "glDisable";
      glDisableClientState = fun_ptr "glDisableClientState";
      glDrawArrays = fun_ptr "glDrawArrays";
      glDrawElements = fun_ptr "glDrawElements";     
      glDrawRangeElements = fun_ptr "glDrawRangeElements";
      glEnable = fun_ptr "glEnable";
      glEnableClientState = fun_ptr "glEnableClientState";
      glFinish = fun_ptr "glFinish";
      glFlush = fun_ptr "glFlush";
      glGenBuffersARB = fun_ptr "glGenBuffersARB";
      glGenTextures = fun_ptr "glGenTextures";
      glGetBufferSubDataARB = fun_ptr "glGetBufferSubDataARB";
      glGetError = fun_ptr "glGetError";
      glGetIntegerv = fun_ptr "glGetIntegerv";
      glGetTexParameterfv = fun_ptr "glGetTexParameterfv";
      glGetTexParameteriv = fun_ptr "glGetTexParameteriv";
      glLoadMatrixd = fun_ptr "glLoadMatrixd";
      glMapBufferARB = fun_ptr "glMapBufferARB";
      glMatrixMode = fun_ptr "glMatrixMode";
      glMultMatrixd = fun_ptr "glMultMatrixd";
      glNormalPointer = fun_ptr "glNormalPointer";
      glPixelStorei = fun_ptr "glPixelStorei";
      glPolygonMode = fun_ptr "glPolygonMode";
      glPolygonOffset = fun_ptr "glPolygonOffset";
      glReadPixels = fun_ptr "glReadPixels";
      glScissor = fun_ptr "glScissor";
      glShadeModel = fun_ptr "glShadeModel";
      glStencilFunc = fun_ptr "glStencilFunc";
      glStencilMask = fun_ptr "glStencilMask";
      glStencilOp = fun_ptr "glStencilOp";
      glTexCoordPointer = fun_ptr "glTexCoordPointer";
      glTexImage1D = fun_ptr "glTexImage1D";
      glTexImage2D = fun_ptr "glTexImage2D";
      glTexImage3D = fun_ptr "glTexImage3D";
      glTexParameterf = fun_ptr "glTexParameterf";
      glTexParameterfv = fun_ptr "glTexParameterfv";
      glTexParameteri = fun_ptr "glTexParameteri";
      glTexParameteriv = fun_ptr "glTexParameteriv";
      glTexSubImage1D = fun_ptr "glTexSubImage1D";
      glTexSubImage2D = fun_ptr "glTexSubImage2D";
      glTexSubImage3D = fun_ptr "glTexSubImage3D";
      glUnmapBufferARB = fun_ptr "glUnmapBufferARB";
      glVertexPointer = fun_ptr "glVertexPointer";
      glViewport = fun_ptr "glViewport"; 
      c = Cache.create (); }
end

module Ext = struct
  let arb_fragment_shader = "GL_ARB_fragment_shader"
  let arb_pixel_buffer_object =  "GL_ARB_pixel_buffer_object"
  let arb_shader_objects = "GL_ARB_shader_objects"
  let arb_shading_language_100 = "GL_ARB_shading_language_100"
  let arb_texture_float = "GL_ARB_texture_float"
  let arb_texture_non_power_of_two = "GL_ARB_texture_non_power_of_two"
  let arb_texture_rectangle = "GL_ARB_texture_rectangle"
  let arb_vertex_buffer_object = "GL_ARB_vertex_buffer_object"
  let arb_vertex_shader = "GL_ARB_vertex_shader"
  let ati_texture_float = "GL_ATI_texture_float"
  let ext_texture_compression_s3tc = "GL_EXT_texture_compression_s3tc"
end



(* Functions *)
external _active_texture : Ptr.f -> Enum.tex_unit -> unit = "lit_glActiveTexture"
external _alpha_func : Ptr.f -> Enum.func -> float -> unit = "lit_glAlphaFunc"
external _bind_buffer : Ptr.f -> Enum.buf_target -> Ptr.g -> unit = "lit_glBindBufferARB"
external _bind_texture : Ptr.f -> Enum.tex_target -> Ptr.g  -> unit = "lit_glBindTexture"
external _blend_func : Ptr.f -> Enum.blend_factor -> Enum.blend_factor -> unit = "lit_glBlendFunc"
external _buffer_data : Ptr.f -> Enum.buf_target -> unit:int -> size:int -> Ptr.c -> Enum.buf_usage -> unit = "lit_bc_glBufferDataARB" "lit_glBufferDataARB"
external _buffer_sub_data : Ptr.f -> Enum.buf_target -> unit:int -> offset:int -> size:int -> Ptr.c -> unit = "lit_bc_glBufferSubDataARB" "lit_glBufferSubDataARB"
external _clear : Ptr.f -> Enum.buffer_bit_field -> unit = "lit_glClear"
external _clear_accum : Ptr.f -> float -> float -> float -> float -> unit = "lit_glClearAccum"
external _clear_color : Ptr.f -> float -> float -> float -> float -> unit = "lit_glClearColor"
external _clear_depth : Ptr.f -> float -> unit = "lit_glClearDepth"
external _clear_stencil : Ptr.f -> int32 -> unit = "lit_glClearStencil"
external _color_mask : Ptr.f -> bool -> bool -> bool -> bool -> unit = "lit_glColorMask"
external _color_pointer : Ptr.f -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit = "lit_glColorPointer" (* TODO remove *)
external _cull_face : Ptr.f -> Enum.face -> unit = "lit_glCullFace"
external _delete_buffers : Ptr.f -> Ptr.g -> unit = "lit_glDeleteBuffersARB"
external _delete_texture : Ptr.f -> Ptr.g -> unit = "lit_glDeleteTextures"
external _depth_func : Ptr.f -> Enum.func -> unit = "lit_glDepthFunc"
external _depth_mask : Ptr.f -> bool -> unit = "lit_glDepthMask"
external _depth_range : Ptr.f -> float -> float -> unit = "lit_glDepthRange"
external _disable : Ptr.f -> Enum.cap -> unit = "lit_glDisable"
external _disable_client_state : Ptr.f -> Enum.client_cap -> unit = "lit_glDisableClientState"
external _draw_arrays : Ptr.f -> Enum.mode -> first:int -> count:int -> unit = "lit_glDrawArrays"
external _draw_elements : Ptr.f -> Enum.mode -> count:int -> Enum.astorage -> Ptr.c -> unit = "lit_glDrawElements"
external _draw_range_elements : Ptr.f -> Enum.mode -> min:int -> max:int -> count:int -> Enum.astorage -> Ptr.c -> unit = "lit_bc_glDrawRangeElements" "lit_glDrawRangeElements"
external _enable : Ptr.f -> Enum.cap -> unit = "lit_glEnable"
external _enable_client_state : Ptr.f -> Enum.client_cap -> unit = "lit_glEnableClientState"
external _finish : Ptr.f -> unit = "lit_glFinish"
external _flush : Ptr.f -> unit = "lit_glFlush"
external _gen_buffers : Ptr.f -> Ptr.g = "lit_glGenBuffersARB"
external _gen_texture : Ptr.f -> Ptr.g = "lit_glGenTextures"
external _get_buffer_sub_data : Ptr.f -> Enum.buf_target -> unit:int -> offset:int -> size:int -> Ptr.c -> unit = "lit_bc_glGetBufferSubDataARB" "lit_glGetBufferSubDataARB"
external _get_error : Ptr.f -> unit -> Enum.err = "lit_glGetError"
external _get_integer : Ptr.f -> Enum.get_int -> int = "lit_litGetInteger"
external _get_string : Ptr.f -> Enum.get_str -> string = "lit_glGetString"
external _get_tex_parameterf : Ptr.f -> Enum.tex_target -> Enum.tex_paramf -> float = "lit_litGetTexParameterf"
external _get_tex_parameteri : Ptr.f -> Enum.tex_target -> Enum.tex_parami -> int = "lit_litGetTexParameteri"
external _load_matrix : Ptr.f -> Gg.m4 -> unit = "lit_glLoadMatrixd"
external _map_buffer : Ptr.f -> Enum.buf_target -> Enum.buf_access -> Ptr.c = "lit_glMapBufferARB"
external _matrix_mode : Ptr.f -> Enum.matrix_mode -> unit = "lit_glMatrixMode"
external _mult_matrix : Ptr.f -> Gg.m4 -> unit = "lit_glMultMatrixd"
external _normal_pointer : Ptr.f -> Enum.astorage -> stride:int -> Ptr.c -> unit = "lit_glNormalPointer" (* TODO remove *)
external _pixel_storei : Ptr.f -> Enum.pixel_store_param -> int -> unit = "lit_glPixelStorei"
external _polygon_mode : Ptr.f -> Enum.face -> Enum.polygon_mode -> unit = "lit_glPolygonMode"
external _polygon_offset : Ptr.f -> float -> float -> unit = "lit_glPolygonOffset"
external _read_pixels : Ptr.f -> x:int -> y:int -> w:int -> h:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glReadPixels" "lit_glReadPixels"
external _scissor  : Ptr.f -> int -> int -> int -> int -> unit = "lit_glScissor"
external _shade_model : Ptr.f -> Enum.shade_model -> unit  = "lit_glShadeModel"
external _stencil_func : Ptr.f -> Enum.func -> int32 -> int32 -> unit = "lit_glStencilFunc"
external _stencil_mask : Ptr.f -> int32 -> unit = "lit_glStencilMask"
external _stencil_op : Ptr.f -> Enum.stencil_op -> Enum.stencil_op -> Enum.stencil_op -> unit = "lit_glStencilOp"
external _tex_coord_pointer : Ptr.f -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit = "lit_glTexCoordPointer" (* TODO remove *)
external _tex_image_1d : Ptr.f -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexImage1D" "lit_glTexImage1D"
external _tex_image_2d : Ptr.f -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> h:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexImage2D" "lit_glTexImage2D"
external _tex_image_3d : Ptr.f -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> h:int -> d:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexImage3D" "lit_glTexImage3D"
external _tex_parameterf : Ptr.f -> Enum.tex_target -> Enum.tex_paramf -> float -> unit = "lit_glTexParameterf"
external _tex_parameteri : Ptr.f -> Enum.tex_target -> Enum.tex_parami -> int -> unit = "lit_glTexParameteri"
external _tex_sub_image_1d : Ptr.f -> Enum.tex_target -> level:int -> xoff:int -> w:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexSubImage1D" "lit_glTexSubImage1D"
external _tex_sub_image_2d : Ptr.f -> Enum.tex_target -> level:int -> xoff:int -> yoff:int -> w:int -> h:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexSubImage2D" "lit_glTexSubImage2D"
external _tex_sub_image_3d : Ptr.f -> Enum.tex_target -> level:int -> xoff:int -> yoff:int -> zoff:int -> w:int -> h:int -> d:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit = "lit_bc_glTexSubImage3D" "lit_glTexSubImage3D"
external _unmap_buffer : Ptr.f -> Enum.buf_target -> bool = "lit_glUnmapBufferARB"
external _vertex_pointer : Ptr.f -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit = "lit_glVertexPointer" (* TODO remove *)
external _viewport : Ptr.f -> int -> int -> int -> int -> unit = "lit_glViewport"


let active_texture ft = _active_texture ft.Ptr.glActiveTexture
let alpha_func ft = _alpha_func ft.Ptr.glAlphaFunc
let bind_buffer ft = _bind_buffer ft.Ptr.glBindBufferARB
let bind_texture ft = _bind_texture ft.Ptr.glBindTexture
let blend_func ft = _blend_func ft.Ptr.glBlendFunc
let buffer_data ft = _buffer_data ft.Ptr.glBufferDataARB
let buffer_sub_data ft = _buffer_sub_data ft.Ptr.glBufferSubDataARB
let clear ft = _clear ft.Ptr.glClear
let clear_accum ft = _clear_accum ft.Ptr.glClearAccum
let clear_color ft = _clear_color ft.Ptr.glClearColor
let clear_depth ft = _clear_depth ft.Ptr.glClearDepth
let clear_stencil ft = _clear_stencil ft.Ptr.glClearStencil
let color_mask ft = _color_mask ft.Ptr.glColorMask
let color_pointer ft = _color_pointer ft.Ptr.glColorPointer
let cull_face ft = _cull_face ft.Ptr.glCullFace
let delete_buffer ft = _delete_buffers ft.Ptr.glDeleteBuffersARB
let delete_texture ft = _delete_texture ft.Ptr.glDeleteTextures
let depth_func ft = _depth_func ft.Ptr.glDepthFunc
let depth_mask ft = _depth_mask ft.Ptr.glDepthMask
let depth_range ft = _depth_range ft.Ptr.glDepthRange
let disable ft = _disable ft.Ptr.glDisable
let disable_client_state ft = _disable_client_state ft.Ptr.glDisableClientState
let draw_arrays ft = _draw_arrays ft.Ptr.glDrawArrays
let draw_elements ft = _draw_elements ft.Ptr.glDrawElements
let draw_range_elements ft = _draw_range_elements ft.Ptr.glDrawRangeElements
let enable ft = _enable ft.Ptr.glEnable
let enable_client_state ft = _enable_client_state ft.Ptr.glEnableClientState
let finish ft = _finish ft.Ptr.glFinish
let flush ft = _flush ft.Ptr.glFlush
let gen_buffer ft = _gen_buffers ft.Ptr.glGenBuffersARB
let gen_texture ft = _gen_texture ft.Ptr.glGenTextures
let get_buffer_sub_data ft = _get_buffer_sub_data ft.Ptr.glGetBufferSubDataARB
let get_error ft = _get_error ft.Ptr.glGetError
let get_integer ft = _get_integer ft.Ptr.glGetIntegerv
let get_string = _get_string (Ptr.fun_ptr "glGetString")
let get_tex_parameterf ft = _get_tex_parameterf ft.Ptr.glGetTexParameterfv
let get_tex_parameteri ft = _get_tex_parameteri ft.Ptr.glGetTexParameteriv
let load_matrix ft = _load_matrix ft.Ptr.glLoadMatrixd
let map_buffer ft = _map_buffer ft.Ptr.glMapBufferARB 
let matrix_mode ft = _matrix_mode ft.Ptr.glMatrixMode
let mult_matrix ft = _mult_matrix ft.Ptr.glMultMatrixd
let normal_pointer ft = _normal_pointer ft.Ptr.glNormalPointer
let pixel_storei ft = _pixel_storei ft.Ptr.glPixelStorei
let polygon_mode ft = _polygon_mode ft.Ptr.glPolygonMode
let polygon_offset ft = _polygon_offset ft.Ptr.glPolygonOffset
let read_pixels ft = _read_pixels ft.Ptr.glReadPixels
let scissor  ft = _scissor  ft.Ptr.glScissor
let shade_model ft = _shade_model ft.Ptr.glShadeModel
let stencil_func ft = _stencil_func ft.Ptr.glStencilFunc
let stencil_mask ft = _stencil_mask ft.Ptr.glStencilMask
let stencil_op ft = _stencil_op ft.Ptr.glStencilOp
let tex_coord_pointer ft = _tex_coord_pointer ft.Ptr.glTexCoordPointer
let tex_image_1d ft = _tex_image_1d ft.Ptr.glTexImage1D
let tex_image_2d ft = _tex_image_2d ft.Ptr.glTexImage2D
let tex_image_3d ft = _tex_image_3d ft.Ptr.glTexImage3D
let tex_parameterf ft = _tex_parameterf ft.Ptr.glTexParameterf
let tex_parameteri ft = _tex_parameteri ft.Ptr.glTexParameteri
let tex_sub_image_1d ft = _tex_sub_image_1d ft.Ptr.glTexSubImage1D
let tex_sub_image_2d ft = _tex_sub_image_2d ft.Ptr.glTexSubImage2D
let tex_sub_image_3d ft = _tex_sub_image_3d ft.Ptr.glTexSubImage3D
let unmap_buffer ft = _unmap_buffer ft.Ptr.glUnmapBufferARB
let vertex_pointer ft = _vertex_pointer ft.Ptr.glVertexPointer
let viewport ft = _viewport ft.Ptr.glViewport                

let split_string s sep =
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

let extract_version s =    (* Extracts major and minor version number *)
  let err_parse_version s = Printf.sprintf "unable to parse version: '%s'" s in
  let v = try String.sub s 0 (String.index s ' ') with Not_found -> s in 
  let nums = try List.map int_of_string (split_string v '.') 
  with Failure _ -> failwith (err_parse_version s)
  in
  match nums with
  | [ maj; min ] -> maj, min
  | [ maj; min; _] -> maj, min
  | _ -> failwith (err_parse_version s)


external lit_draw_tex_rect : Gg.v2 -> unit = "lit_draw_tex_rect" (* TODO remove *)
let lit_tex_unit i = GL_TEXTURE0 + i
external lit_debug : Ptr.c -> unit = "lit_debug"
let lit_set_buffer_bit b b' = b lor b'
	
let lit_extensions ft = 
  List.sort compare (split_string (get_string extensions) ' ')

let lit_version vs = 
  let version = get_string vs in
  let vnum = extract_version version in
  version, vnum

let lit_error_to_string = function
  | e when e = no_error -> "no error"
  | e when e = invalid_enum -> "GLenum argument out of range"
  | e when e = invalid_value -> "numeric argument out of range"
  | e when e = invalid_operation -> "operation illegal in current state"
  | e when e = stack_overflow -> "command would cause a stack overflow"
  | e when e = stack_underflow -> "command would cause a stack underflow"
  | e when e = out_of_memory -> "not enough memory left to execute command"
  | e when e = table_too_large -> "the specified table is too large"
  | _ -> "unknown"

(* Gl cache *)
module Cache = struct
  let active_texture ft i =
    if (i != ft.Ptr.c.Ptr.Cache.active_texture) then 
      (active_texture ft i; ft.Ptr.c.Ptr.Cache.active_texture <- i)

  let matrix_mode ft m =
    if (m != ft.Ptr.c.Ptr.Cache.matrix_mode) then 
      (matrix_mode ft m; ft.Ptr.c.Ptr.Cache.matrix_mode <- m)
end
