* FINALIZERS, don't call GL ! no guarantee on thread.
* `Uniform.viewport_{o,size}` should we still have them in 
  normalized surface coordinate and have a seperate `surface_size`
  builtin ? 
* Redo uniform handling, also Effect has ~uniforms and Prog ~uset  
* Use Lequal as default depth test.
* s/shader_kind/shader_stage
* Add depth_range to View.
* View is not what we want yet I think. I should be easy to change 
  the aspect e.g. maybe specify the projection through a variant.
* Review the Uniform module, it's not that good I think. 
* Interface mostly untyped. Explore the possibility of using 
  row polymorphism to match geometry against effects
* Support for different shaders according to backend in effect. 
  Maybe not, leave that to the client and module system. 
* Support for base vertex rendering. Prim.
* Support for explicit resource allocate/disposal in the 
  Renderer.{Buf,Prog,Tex} modules. For now it's allocate on usage and 
  and dispose in Gc.finalizer.
* Tex format
* Multipass (or not) 
* Transform feedback 
* Renderer.Caps.
* Renderer.Stats.
* Renderer.Buf.{blit,get}
* Source-level meta programming, embbed a small shading language in
  OCaml.
* Buf.gpu_upload -> gpu_update ? Make it consistent with Tex.
  Also do we want to allow setting the value to false ?
* Renderer.Uniform.builtin : Uniform.builtin -> value_untyped 
  (* For debugging *) 
* Review pretty printers. 
* glEnable (GL_MULTISAMPLE_ARB)
* Update examples and litu to "new style" specify atts prim first
  then generate the data. Makes more sense as you get to learn the
  actual layout before reading the generating code.
* Review sample_format to gl types. 
