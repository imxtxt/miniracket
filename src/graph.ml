open Base

type t = { nodes : (string, string Hash_set.t) Hashtbl.t }

let graph_new () = { nodes = Hashtbl.create (module String) }

let add_node t n =
  if not (Hashtbl.mem t.nodes n) then
    Hashtbl.add_exn t.nodes ~key:n ~data:(Hash_set.create (module String))

let add_nodes t ns = List.iter ns ~f:(fun n -> add_node t n)

let add_edge t n1 n2 =
  let open String in
  assert (n1 <> n2);
  let n1_adjs = Hashtbl.find_exn t.nodes n1 in
  let n2_adjs = Hashtbl.find_exn t.nodes n2 in
  Hash_set.add n1_adjs n2;
  Hash_set.add n2_adjs n1

let adjs t (n : string) = Hashtbl.find_exn t.nodes n |> Hash_set.to_list
