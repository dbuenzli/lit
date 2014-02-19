(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.
  ----------------------------------------------------------------------------*)

(** Raw OpenGL bindings. 

   C names are transformed as follows. Drop the [gl] prefix,
    add an underscore between each minuscule and majuscule and
    lowercase the result (e.g. [glTheFun] maps to
   [the_fun]). Exceptions are due to name clashes. Functions that do
   not map directly to an OpenGL one are prefixed by [lit].

    Most enumerant arguments of gl functions are constrained by
    {!Gl.Enum} types, however there may be exceptions. Correct use for
    these exceptions should be enforced by our usage in in lit. This
    means that if an invalid enumerant OpenGL error occurs this could
    be due to an internal misusage of OpenGL (TODO check that this is
    really true).  *)

(** Enumerant types *)
module Enum : sig
  type astorage (** Attribute storage *)
  type blend_factor
  type buf_access
  type buf_target
  type buf_usage
  type buffer_bit_field
  type cap
  type client_cap
  type err 
  type face
  type func
  type get_int
  type get_str
  type mode
  type pformat 
  type pixel_store_param
  type polygon_mode
  type pstorage (** Pixel storage *)
  type shade_model
  type stencil_op
  type tex_paramf
  type tex_parami
  type tex_pformat
  type tex_target
  type tex_unit
  type matrix_mode 
end


(** Gpu, cpu and function pointers. *)
module Ptr : sig 

  type g (** Gpu pointer. *)
  val g_nil : g (** Gpu nil pointer. *)

  type c (** Cpu pointer. *)
  val c_nil : c (** Cpu nil pointer. *)
  external c_offset : c -> unit:int -> offset:int -> c = "lit_c_offset"
  (** Final offset is [unit * offset] *)
  external c_copy : src:c -> dst:c -> unit:int -> len:int -> unit= "lit_c_copy"
  (** Final length is [unit * len] *)

  type ('a, 'b) barray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
  external c_of_barray   : ('a, 'b) barray -> c = "lit_c_of_barray"
  external barray_of_c   : c -> ('a, 'b) Bigarray.kind -> len:int ->
    ('a, 'b) barray =  "lit_barray_of_c"
  external barray_invalidate : ('a, 'b) barray -> unit = "lit_barray_invalidate"

  type f (** Function pointer. *)
  type ft (** Function pointer table (one per context). *)
  val fun_table : unit -> ft
  (** Returns the function pointer table of the current context. *)
end

(** Extension strings *)
module Ext : sig
  val arb_fragment_shader : string
  val arb_pixel_buffer_object : string 
  val arb_shader_objects : string
  val arb_shading_language_100 : string
  val arb_texture_float : string
  val arb_texture_non_power_of_two : string
  val arb_texture_rectangle : string
  val arb_vertex_buffer_object : string
  val arb_vertex_shader : string
  val ati_texture_float : string
  val ext_texture_compression_s3tc : string
end


(** {2 Enumerant constants} *)

val abyte : Enum.astorage
val accum_alpha_bits : Enum.get_int
val accum_blue_bits : Enum.get_int
val accum_buffer_bit : Enum.buffer_bit_field
val accum_green_bits : Enum.get_int
val accum_red_bits : Enum.get_int
val adouble : Enum.astorage
val afloat : Enum.astorage
val aint : Enum.astorage
val alpha : Enum.pformat
val alpha12 : Enum.tex_pformat
val alpha16 : Enum.tex_pformat
val alpha16f : Enum.tex_pformat
val alpha32f : Enum.tex_pformat
val alpha4 : Enum.tex_pformat
val alpha8 : Enum.tex_pformat
val alpha_bits : Enum.get_int
val alpha_test : Enum.cap
val always : Enum.func
val array_buffer : Enum.buf_target
val ashort : Enum.astorage
val aunsigned_byte : Enum.astorage
val aunsigned_int : Enum.astorage
val aunsigned_short : Enum.astorage
val back : Enum.face
val bgr : Enum.pformat
val bgra : Enum.pformat
val blend : Enum.cap
val blend_factor_one : Enum.blend_factor 
val blend_factor_zero : Enum.blend_factor 
val blue : Enum.pformat
val blue_bits : Enum.get_int
val cap_cull_face : Enum.cap
val clamp : int
val clamp_to_border : int
val clamp_to_edge : int
val color_array : Enum.client_cap 
val color_buffer_bit : Enum.buffer_bit_field
val color_index : Enum.pformat
val compressed_rgb_s3tc_dxt1 : Enum.tex_pformat
val compressed_rgba_s3tc_dxt1 : Enum.tex_pformat
val compressed_rgba_s3tc_dxt3 : Enum.tex_pformat
val compressed_rgba_s3tc_dxt5 : Enum.tex_pformat
val decr : Enum.stencil_op
val depth_bits : Enum.get_int
val depth_buffer_bit : Enum.buffer_bit_field
val depth_component : Enum.pformat
val depth_component16 : Enum.tex_pformat
val depth_component24 : Enum.tex_pformat
val depth_component32 : Enum.tex_pformat
val depth_test : Enum.cap
val depth_texture_mode : Enum.tex_parami
val dst_alpha : Enum.blend_factor 
val dst_color : Enum.blend_factor 
val dynamic_copy : Enum.buf_usage
val dynamic_draw : Enum.buf_usage
val dynamic_read : Enum.buf_usage
val element_array_buffer : Enum.buf_target
val equal : Enum.func
val extensions : Enum.get_str
val fill  : Enum.polygon_mode
val flat : Enum.shade_model
val front : Enum.face
val front_and_back : Enum.face
val generate_mipmap : Enum.tex_parami
val gequal : Enum.func
val greater : Enum.func
val green : Enum.pformat
val green_bits : Enum.get_int
val incr : Enum.stencil_op
val intensity12 : Enum.tex_pformat
val intensity16 : Enum.tex_pformat
val intensity16 : Enum.tex_pformat
val intensity16f : Enum.tex_pformat
val intensity32f : Enum.tex_pformat
val intensity4 : Enum.tex_pformat
val intensity8 : Enum.tex_pformat
val invalid_enum : Enum.err
val invalid_operation : Enum.err
val invalid_value : Enum.err
val invert : Enum.stencil_op
val keep : Enum.stencil_op
val lequal : Enum.func
val less : Enum.func
val line  : Enum.polygon_mode
val line_loop : Enum.mode
val line_strip : Enum.mode
val linear : int
val linear_mipmap_linear : int
val linear_mipmap_nearest : int
val lines : Enum.mode
val lit_zero_buffer_bit : Enum.buffer_bit_field
val luminance : Enum.pformat
val luminance12 : Enum.tex_pformat
val luminance12_alpha12 : Enum.tex_pformat
val luminance16 : Enum.tex_pformat
val luminance16_alpha16 : Enum.tex_pformat
val luminance16f : Enum.tex_pformat
val luminance32f : Enum.tex_pformat
val luminance4 : Enum.tex_pformat
val luminance4_alpha4 : Enum.tex_pformat
val luminance6_alpha6 : Enum.tex_pformat
val luminance8 : Enum.tex_pformat
val luminance8_alpha8 : Enum.tex_pformat
val luminance_alpha : Enum.pformat
val luminance_alpha16f : Enum.tex_pformat
val luminance_alpha32f : Enum.tex_pformat
val max_3d_texture_size : Enum.get_int
val max_clip_planes : Enum.get_int
val max_combined_texture_image_units : Enum.get_int
val max_elements_indices : Enum.get_int
val max_elements_vertices : Enum.get_int
val max_fragment_texture_image_units : Enum.get_int
val max_fragment_uniform_components : Enum.get_int
val max_texture_size : Enum.get_int
val max_varying_floats : Enum.get_int
val max_vertex_attribs : Enum.get_int
val max_vertex_texture_image_units : Enum.get_int
val max_vertex_uniform_components : Enum.get_int
val mirrored_repeat : int
val modelview : Enum.matrix_mode
val nearest : int
val nearest_mipmap_linear : int
val nearest_mipmap_nearest : int
val never : Enum.func
val no_error : Enum.err
val normal_array : Enum.client_cap 
val notequal : Enum.func
val one_minus_dst_alpha : Enum.blend_factor 
val one_minus_dst_color : Enum.blend_factor 
val one_minus_src_alpha : Enum.blend_factor 
val one_minus_src_color : Enum.blend_factor 
val out_of_memory : Enum.err
val pack_alignment : Enum.pixel_store_param
val pack_image_height : Enum.pixel_store_param
val pack_lsb_first : Enum.pixel_store_param
val pack_row_length : Enum.pixel_store_param
val pack_skip_images : Enum.pixel_store_param
val pack_skip_pixels : Enum.pixel_store_param
val pack_skip_rows : Enum.pixel_store_param
val pack_swap_bytes : Enum.pixel_store_param
val pbitmap : Enum.pstorage
val pbyte : Enum.pstorage
val pfloat : Enum.pstorage
val pint : Enum.pstorage
val pixel_pack_buffer : Enum.buf_target
val pixel_unpack_buffer : Enum.buf_target
val point : Enum.polygon_mode
val points : Enum.mode
val polygon : Enum.mode
val polygon_offset_fill : Enum.cap
val polygon_offset_line : Enum.cap
val polygon_offset_point : Enum.cap
val projection : Enum.matrix_mode
val pshort : Enum.pstorage
val punsigned_byte : Enum.pstorage 
val punsigned_byte_2_3_3_rev : Enum.pstorage
val punsigned_byte_3_3_2 : Enum.pstorage
val punsigned_int : Enum.pstorage
val punsigned_int_10_10_10_2 : Enum.pstorage
val punsigned_int_2_10_10_10_rev : Enum.pstorage
val punsigned_int_8_8_8_8 : Enum.pstorage
val punsigned_int_8_8_8_8_rev : Enum.pstorage
val punsigned_short : Enum.pstorage
val punsigned_short_1_5_5_5_rev : Enum.pstorage
val punsigned_short_4_4_4_4 : Enum.pstorage
val punsigned_short_4_4_4_4_rev : Enum.pstorage
val punsigned_short_5_5_5_1 : Enum.pstorage
val punsigned_short_5_6_5 : Enum.pstorage
val punsigned_short_5_6_5_rev : Enum.pstorage
val quad_strip : Enum.mode
val quads : Enum.mode
val r3_g3_b2 : Enum.tex_pformat
val read_only : Enum.buf_access
val read_write : Enum.buf_access
val red : Enum.pformat
val red_bits : Enum.get_int
val renderer : Enum.get_str
val repeat : int
val replace : Enum.stencil_op
val rgb : Enum.pformat
val rgb10 : Enum.tex_pformat
val rgb10_a2 : Enum.tex_pformat
val rgb12 : Enum.tex_pformat
val rgb16 : Enum.tex_pformat
val rgb16f : Enum.tex_pformat
val rgb32f : Enum.tex_pformat
val rgb4 : Enum.tex_pformat
val rgb5 : Enum.tex_pformat
val rgb5_a1 : Enum.tex_pformat
val rgb8 : Enum.tex_pformat
val rgba : Enum.pformat
val rgba12 : Enum.tex_pformat
val rgba16 : Enum.tex_pformat
val rgba16f : Enum.tex_pformat
val rgba2 : Enum.tex_pformat
val rgba32f : Enum.tex_pformat
val rgba4 : Enum.tex_pformat
val rgba8 : Enum.tex_pformat
val scissor_test : Enum.cap
val shading_language_version : Enum.get_str
val smooth : Enum.shade_model
val src_alpha : Enum.blend_factor 
val src_alpha_saturate : Enum.blend_factor 
val src_color : Enum.blend_factor 
val stack_overflow : Enum.err
val stack_underflow : Enum.err
val static_copy : Enum.buf_usage
val static_draw : Enum.buf_usage
val static_read : Enum.buf_usage
val stencil_bits : Enum.get_int
val stencil_buffer_bit : Enum.buffer_bit_field
val stencil_index : Enum.pformat
val stencil_test : Enum.cap 
val stream_copy : Enum.buf_usage
val stream_draw : Enum.buf_usage
val stream_read : Enum.buf_usage
val table_too_large : Enum.err
val texture : Enum.matrix_mode 
val texture_1D : Enum.cap
val texture_2D : Enum.cap
val texture_3D : Enum.cap
val texture_base_level : Enum.tex_parami
val texture_compare_func : Enum.tex_parami
val texture_compare_mode : Enum.tex_parami
val texture_coord_array : Enum.client_cap 
val texture_cube_map : Enum.cap
val texture_lod_bias : Enum.tex_paramf
val texture_mag_filter : Enum.tex_parami
val texture_max_level : Enum.tex_parami
val texture_max_lod : Enum.tex_paramf
val texture_min_filter : Enum.tex_parami
val texture_min_lod : Enum.tex_paramf
val texture_priority : Enum.tex_paramf
val texture_rectangle : Enum.cap
val texture_resident : Enum.tex_parami (** only for getting *)
val texture_wrap_r : Enum.tex_parami
val texture_wrap_s : Enum.tex_parami
val texture_wrap_t : Enum.tex_parami
val triangle_fan : Enum.mode
val triangle_strip : Enum.mode
val triangles : Enum.mode
val ttexture_1D : Enum.tex_target
val ttexture_2D : Enum.tex_target
val ttexture_3D : Enum.tex_target
val ttexture_cube_map : Enum.tex_target
val ttexture_cube_map_negative_x : Enum.tex_target
val ttexture_cube_map_negative_y : Enum.tex_target
val ttexture_cube_map_negative_z : Enum.tex_target
val ttexture_cube_map_positive_x : Enum.tex_target
val ttexture_cube_map_positive_y : Enum.tex_target
val ttexture_cube_map_positive_z : Enum.tex_target
val ttexture_rectangle : Enum.tex_target
val unpack_alignment : Enum.pixel_store_param
val unpack_image_height : Enum.pixel_store_param
val unpack_lsb_first : Enum.pixel_store_param
val unpack_row_length : Enum.pixel_store_param
val unpack_skip_images : Enum.pixel_store_param
val unpack_skip_pixels : Enum.pixel_store_param
val unpack_skip_rows : Enum.pixel_store_param
val unpack_swap_bytes : Enum.pixel_store_param
val vendor : Enum.get_str
val version : Enum.get_str
val vertex_array : Enum.client_cap 
val write_only : Enum.buf_access
val zero : Enum.stencil_op

(** {2 Functions} *)

val active_texture : Ptr.ft -> Enum.tex_unit -> unit 
val alpha_func : Ptr.ft -> Enum.func -> float -> unit 
val bind_buffer : Ptr.ft -> Enum.buf_target -> Ptr.g -> unit
val bind_texture : Ptr.ft -> Enum.tex_target -> Ptr.g  -> unit
val blend_func : Ptr.ft -> Enum.blend_factor -> Enum.blend_factor -> unit 
val buffer_data : Ptr.ft -> Enum.buf_target -> unit:int -> size:int -> Ptr.c -> Enum.buf_usage -> unit
val buffer_sub_data : Ptr.ft -> Enum.buf_target -> unit:int -> offset:int -> size:int -> Ptr.c -> unit 
val clear : Ptr.ft -> Enum.buffer_bit_field -> unit 
val clear_accum : Ptr.ft -> float -> float -> float -> float -> unit 
val clear_color : Ptr.ft -> float -> float -> float -> float -> unit 
val clear_depth : Ptr.ft -> float -> unit 
val clear_stencil : Ptr.ft -> int32 -> unit 
val color_mask : Ptr.ft -> bool -> bool -> bool -> bool -> unit 
val color_pointer : Ptr.ft -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit 
val cull_face : Ptr.ft -> Enum.face -> unit 
val delete_buffer : Ptr.ft -> Ptr.g -> unit 
val delete_texture : Ptr.ft -> Ptr.g -> unit
val depth_func : Ptr.ft -> Enum.func -> unit 
val depth_mask : Ptr.ft -> bool -> unit 
val depth_range : Ptr.ft -> float -> float -> unit 
val disable : Ptr.ft -> Enum.cap -> unit
val disable_client_state : Ptr.ft -> Enum.client_cap -> unit 
val draw_arrays : Ptr.ft -> Enum.mode -> first:int -> count:int -> unit
val draw_elements : Ptr.ft -> Enum.mode -> count:int -> Enum.astorage -> Ptr.c -> unit
val draw_range_elements : Ptr.ft -> Enum.mode -> min:int -> max:int -> count:int -> Enum.astorage -> Ptr.c -> unit
val enable : Ptr.ft -> Enum.cap -> unit 
val enable_client_state : Ptr.ft -> Enum.client_cap -> unit 
val finish : Ptr.ft -> unit 
val flush : Ptr.ft -> unit 
val gen_buffer : Ptr.ft -> Ptr.g 
val gen_texture : Ptr.ft -> Ptr.g 
val get_buffer_sub_data : Ptr.ft -> Enum.buf_target -> unit:int -> offset:int -> size:int -> Ptr.c -> unit 
val get_error : Ptr.ft -> unit -> Enum.err
val get_integer : Ptr.ft -> Enum.get_int -> int
val get_string : Enum.get_str -> string
val get_tex_parameterf : Ptr.ft -> Enum.tex_target -> Enum.tex_paramf -> float
val get_tex_parameteri : Ptr.ft -> Enum.tex_target -> Enum.tex_parami -> int
val load_matrix : Ptr.ft -> Gg.m4 -> unit
val map_buffer : Ptr.ft -> Enum.buf_target -> Enum.buf_access -> Ptr.c
val matrix_mode : Ptr.ft -> Enum.matrix_mode -> unit
val mult_matrix : Ptr.ft -> Gg.m4 -> unit
val normal_pointer : Ptr.ft -> Enum.astorage -> stride:int -> Ptr.c -> unit 
val pixel_storei : Ptr.ft -> Enum.pixel_store_param -> int -> unit 
val polygon_mode : Ptr.ft -> Enum.face -> Enum.polygon_mode -> unit 
val polygon_offset : Ptr.ft -> float -> float -> unit 
val read_pixels : Ptr.ft -> x:int -> y:int -> w:int -> h:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit 
val scissor  : Ptr.ft -> int -> int -> int -> int -> unit 
val shade_model : Ptr.ft -> Enum.shade_model -> unit 
val stencil_func : Ptr.ft -> Enum.func -> int32 -> int32 -> unit
val stencil_mask : Ptr.ft -> int32 -> unit 
val stencil_op : Ptr.ft -> Enum.stencil_op -> Enum.stencil_op -> Enum.stencil_op-> unit 
val tex_coord_pointer : Ptr.ft -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit 
val tex_image_1d : Ptr.ft -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit
val tex_image_2d : Ptr.ft -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> h:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit
val tex_image_3d : Ptr.ft -> Enum.tex_target -> level:int -> Enum.tex_pformat -> w:int -> h:int -> d:int -> border:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit 
val tex_parameterf : Ptr.ft -> Enum.tex_target -> Enum.tex_paramf -> float -> unit
val tex_parameteri : Ptr.ft -> Enum.tex_target -> Enum.tex_parami -> int -> unit
val tex_sub_image_1d : Ptr.ft -> Enum.tex_target -> level:int -> xoff:int -> w:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit
val tex_sub_image_2d : Ptr.ft -> Enum.tex_target -> level:int -> xoff:int -> yoff:int -> w:int -> h:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit
val tex_sub_image_3d : Ptr.ft -> Enum.tex_target -> level:int -> xoff:int -> yoff:int -> zoff:int -> w:int -> h:int -> d:int -> Enum.pformat -> Enum.pstorage -> Ptr.c -> unit 
val unmap_buffer : Ptr.ft -> Enum.buf_target -> bool
val vertex_pointer : Ptr.ft -> size:int -> Enum.astorage -> stride:int -> Ptr.c -> unit 
val viewport : Ptr.ft -> int -> int -> int -> int -> unit 



external lit_debug : Ptr.c -> unit = "lit_debug"
val lit_draw_tex_rect : Gg.v2 -> unit 
val lit_error_to_string : Enum.err -> string
val lit_extensions : unit -> string list
val lit_set_buffer_bit : Enum.buffer_bit_field -> Enum.buffer_bit_field -> 
  Enum.buffer_bit_field
val lit_tex_unit : int -> Enum.tex_unit
val lit_version : Enum.get_str -> string * (int * int)


(** Cached functions calls for bindings *)
module Cache : sig
  val active_texture : Ptr.ft -> Enum.tex_unit -> unit
  val matrix_mode : Ptr.ft -> Enum.matrix_mode -> unit
end
