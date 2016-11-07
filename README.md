Lit — Lightweight OpenGL-based rendering engine for OCaml
-------------------------------------------------------------------------------
%%VERSION%%

Lit is a lightweight OpenGL-based rendering engine for OCaml. It
provides a thin abstraction to program GPUs with OpenGL, OpenGL ES or
WebGL.

Lit depends on [gg][gg]. The OpenGL and OpenGL ES renderers depend on
[tgls][tgls]. The WebGL renderer on [js_of_ocaml][jsoo]. Lit and its
renderers are distributed under the ISC license.
  
[gg]: http://erratique.ch/software/gg
[tgls]: http://erratique.ch/software/tgls
[jsoo]: http://ocsigen.org/js_of_ocaml/ 

Home page: http://erratique.ch/software/lit  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Lit can be installed with `opam`:

    opam install gg tgls js_of_ocaml lit   # All renderers
    opam install gg tgls lit               # OpenGL and OpenGL ES renderers 
    opam install gg js_of_ocaml lit        # WebGL renderer
    opam install gg lit                    # No renderer
    
If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.


## Documentation

The documentation and API reference is automatically generated by from
the source interfaces. It can be consulted [online][doc] or via
`odig doc lit`.

[doc]: http://erratique.ch/software/lit/doc/

## Sample programs

If you installed Lit with `opam` sample programs are located in the
directory `opam config var lit:doc`. Their source has a comment on
how to compile them.

In the distribution sample programs and tests are located in the
[`test`](test) directory of the distribution. You'll need [useri][useri]
to compile them. They can be built and listed with:

    topkg build --tests true && topkg test --list

[useri]: http://erratique.ch/software/useri/
