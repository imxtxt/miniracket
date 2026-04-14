module MapS = Map.Make (String)
module SetS = Set.Make (String)
module SetI = Set.Make (Int)

let pp_sets formatter sets =
  Format.fprintf formatter "{";
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter ", ")
    (fun formatter var -> Format.fprintf formatter "%s" var)
    formatter (SetS.elements sets);
  Format.fprintf formatter "}"

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

let mangle_label label = label
let align_stack bytes = if bytes mod 16 = 0 then bytes else bytes + 8
