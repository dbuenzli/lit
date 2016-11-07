#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let tgls = Conf.with_pkg "tgls"
let jsoo = Conf.with_pkg "js_of_ocaml"

let example ex =
  Pkg.flatten [
    Pkg.test ~run:false ex;
    Pkg.doc (ex ^ ".ml"); ]

let () =
  Pkg.describe "lit" @@ fun c ->
  let tgls = Conf.value c tgls in
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/lit.mllib";
       Pkg.mllib "src/litu.mllib";
       Pkg.mllib ~cond:tgls "src/lit_gl.mllib";
       Pkg.mllib ~cond:tgls "src/lit_gles.mllib";
       Pkg.mllib ~cond:jsoo "src/lit_webgl.mllib";
       example "test/tri";
       example "test/vortex";
       example "test/sinex";
       example "test/checkboard";
       Pkg.test ~run:false "test/blur";
       Pkg.test ~run:false "test/hiresdump";
       Pkg.test ~run:false "test/diffuse";
       Pkg.test ~run:false "test/wireframe";
 ]
