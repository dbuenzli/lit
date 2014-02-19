Lit — Lightweight OpenGL-based rendering engine for OCaml
-------------------------------------------------------------------------------
Release %%VERSION%%

Lit is a lightweight OpenGL-based rendering engine for OCaml. It
provides a thin abstraction to program GPUs with with OpenGL (3.X),
OpenGL ES (2) or WebGL.

Lit depends on [gg][1]. The OpenGL 3.X, 4.X and ES 2 renderers depend
on [tgls][2]. The WebGL renderer on [js_of_ocaml][3]. Lit and its
renderers are distributed under the BSD3 license.
  
[1]: http://erratique.ch/software/gg
[2]: http://erratique.ch/software/tgls
[3]: http://ocsigen.org/js_of_ocaml/ 

Home page: http://erratique.ch/software/lit  
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Lit can be installed with `opam`:

    opam install gg tgls js_of_ocaml lit   # All renderers
    opam install gg tgls lit               # OpenGL {3,4,ES 2} renderers 
    opam install gg js_of_ocaml lit        # WebGL renderer
    opam install gg lit                    # No renderer
    
If you don't use `opam` consult the [`opam`](opam) file for
build instructions and a complete specification of the dependencies.


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][5] and
there is a generated version in the `doc` directory of the
distribution.

[5]: http://erratique.ch/software/lit/doc/


## Sample programs

If you installed Lit with `opam` sample programs are located in the
directory `opam config var lit:doc`. Their source has a comment on
how to compile them.

In the distribution sample programs are located in the `test`
directory. You'll need [tsdl][6] to compile them.  Sample programs are
located in the `test` directory of the distribution. They can be built
with:

    ocamlbuild -use-ocamlfind tests.otarget

The resulting binaries are in `_build/test` :

[6]: http://erratique.ch/software/tsdl/
