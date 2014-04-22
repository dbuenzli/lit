
* `Lig_gl` raster, depth, blend state setup, compare with 
   the old value before submitting GL calls.
* `Lit_gl` cache bound framebuffer to avoid framebuffer bindings.
* `Uniform.viewport_{o,size}` should we still have them in normalized
  surface coordinate and have a seperate `surface_size` builtin ?
* `Uniform`, rethink uniform handling, also Effect has ~uniforms and Prog ~uset. 
* `Effect.depth_default`, use `Lequal` as default depth test ? 
* `View`, add depth_range. 
* `View`, is not what we want yet I think. I should be easy to change 
  the aspect e.g. maybe specify the projection through a variant.
* `View`, do we really want to bundle the viewport with the projection? 
* `Effect`, support for different shaders according to backend. Maybe
   not, leave that to the client and module system. 
* `Effect`, multipass (or not)
* `Prim`, support for base vertex rendering.
* `Renderer` support for transform feedback 
* Support for explicit GPU resource allocate/disposal in the
  `Buf`,`Prog`,`Prim`,`Tex`,`Fbuf` modules. For now it's allocate on usage 
  and disposal in `Gc.finalizer` + call to `Render.render`. Introduce in each 
  module: 
  ```ocaml
  gpu_prepare  : renderer -> t -> unit 
  gpu_dispose : renderer -> t -> unit
  ```
* `Buf.gpu_upload` -> `gpu_update` ? Make it consistent with Tex.
  Also do we want to allow setting the value to false ?
* Renderer.Uniform.builtin : Uniform.builtin -> value_untyped 
  (* For debugging *) WHAT ?
* Review pretty printers. 
* glEnable (GL_MULTISAMPLE_ARB)
* Update examples and litu to "new style" specify atts prim first
  then generate the data. Makes more sense as you get to learn the
  actual layout before reading the generating code.
* Review sample_format to gl types. 
* Want more control on how ops are going to be drawn. This goes 
  in pair with multipass and opacity and view setup. 
  Maybe we should leave the sorting to higher-level abstractions and
  Renderer should be immediate mode. 
* Test fb with multiple color attachements. 
* Uniform buffers (?)

# Well beyond 

* Source-level meta programming, embbed a small shading language in
  OCaml.
* Interface mostly untyped. Explore the possibility of using 
  row polymorphism to match primitive attributes against effects

