
open Lit

let pp = Format.fprintf 

let pp_attr r ppf attr = 
  let buf = Attr.buf attr in 
  let ba = Renderer.Buf.map r `R buf Bigarray.float32 in 
  let pp_scalar ppf v = pp ppf "%g" v in 
  let pp_ba ppf ba = Ba.pp 
      ~stride:(Attr.stride attr) ~first:(Attr.first attr)
      ~dim:(Attr.dim attr) ~pp_scalar ppf ba
  in
  pp ppf "@[<1>%a@ @[%a@]@]@," Attr.pp attr pp_ba ba; 
  Renderer.Buf.unmap r buf

let pp_index r ppf p = match (Prim.index p) with
| None -> () 
| Some i -> 
    let ba = Renderer.Buf.map r `R i Bigarray.int8_unsigned in 
    let pp_scalar ppf v = pp ppf "%d" v in
    let dim = match Prim.kind p with 
    | `Triangles -> 3 
    | `Lines -> 2 
    | _ -> 1
    in
    let pp_ba ppf ba = Ba.pp ~dim ~pp_scalar ppf ba in 
    pp ppf "@[<v><index>@,%a@]@," pp_ba ba;
    Renderer.Buf.unmap r i

let prim r ppf p =
  pp ppf "@[<v>%a@,%a" Prim.pp p (pp_index r) p; 
  Prim.iter (pp_attr r ppf) p;
  pp ppf "@]@."
