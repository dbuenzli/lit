
let dump_prim r p = 
  let dump_attr attr = 
    let buf = Attr.buf attr in
    let ba = Renderer.Buf.map r `R buf Bigarray.float32 in 
    Printf.printf "%s\n" (Attr.name attr); 
    for i = 0 to (Bigarray.Array1.dim ba) - 1 do 
      Printf.printf "%f " ba.{i}; 
    done; 
    Printf.printf "\n%!";
    Renderer.Buf.unmap r buf
  in
  let dump_index p = match Prim.index p with 
  | None -> Printf.printf "no index" 
  | Some i -> 
      Printf.printf "Index\n"; 
      let ba = Renderer.Buf.map r `R i Bigarray.int8_unsigned in
      for i = 0 to (Bigarray.Array1.dim ba) - 1 do 
        Printf.printf "%d " ba.{i};
      done; 
      Printf.printf "\n%!"; 
      Renderer.Buf.unmap r i
  in
  dump_index p; 
  Prim.attrs_iter p dump_attr
