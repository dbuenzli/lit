#!/usr/bin/env ocaml 
#directory "pkg";;
#use "topkg.ml";;

let tgls = Env.bool "tgls"
let jsoo = Env.bool "jsoo"
let () = 
  Pkg.describe "lit" ~builder:`OCamlbuild [
    Pkg.lib "pkg/META";
    Pkg.lib ~exts:Exts.module_library "src/lit"; 
    Pkg.lib ~exts:Exts.module_library "src/litu";
    Pkg.lib ~cond:tgls ~exts:Exts.module_library "src/lit_gl"; 
    Pkg.lib ~cond:tgls ~exts:Exts.module_library "src/lit_gles";
    Pkg.lib ~cond:jsoo ~exts:Exts.module_library "src/lit_webgl";
    Pkg.doc "README.md";
    Pkg.doc "CHANGES.md"; ]



