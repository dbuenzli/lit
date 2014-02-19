(*----------------------------------------------------------------------------
   Copyright (c) 2007, Daniel C. Bünzli. All rights reserved.
   Distributed under a BSD license, see ../LICENSE.                            
  ----------------------------------------------------------------------------*)

type kind = [ 
  | `Points | `Lines | `Line_loop | `Line_strip | `Triangles 
  | `Triangle_strip | `Triangle_fan | `Quads | `Quad_strip ]
