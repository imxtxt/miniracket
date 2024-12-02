module MapS = Map.Make (String)
module SetS = Set.Make (String)

let gensym =
  let i = ref 0 in
  fun () ->
    incr i;
    Printf.sprintf "Var%d" !i

let genlabel =
  let i = ref 0 in
  fun () ->
    incr i;
    Printf.sprintf "Block_%d" !i

let mangle_label label = "_" ^ label
let align_stack bytes = if bytes mod 16 = 0 then bytes else bytes + 8
