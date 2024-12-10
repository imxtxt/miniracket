open Utilities
module A = Ast_mon
module I = Ir

let locals_types = Hashtbl.create 10

let to_ir_atom (atom : A.atom) : I.atom =
  match atom with
  | Int num -> Int num
  | Var var -> Var var

let rec explicate_tail { A.exp; _ } =
  match exp with
  | A.Int num -> I.Return (I.Int num)
  | A.Read -> I.Return I.Read
  | A.Add (a1, a2) -> I.Return (I.Add (to_ir_atom a1, to_ir_atom a2))
  | A.Sub (a1, a2) -> I.Return (I.Sub (to_ir_atom a1, to_ir_atom a2))
  | A.Var var -> I.Return (I.Var var)
  | A.Let (var, init, body) ->
      Hashtbl.add locals_types var init.ty;
      let cont = explicate_tail body in
      explicate_assign init var cont

and explicate_assign { A.exp; _ } var cont =
  match exp with
  | A.Int num -> I.Seq (I.Assign (var, I.Int num), cont)
  | A.Read -> I.Seq (I.Assign (var, I.Read), cont)
  | A.Add (a1, a2) ->
      I.Seq (I.Assign (var, I.Add (to_ir_atom a1, to_ir_atom a2)), cont)
  | A.Sub (a1, a2) ->
      I.Seq (I.Assign (var, I.Sub (to_ir_atom a1, to_ir_atom a2)), cont)
  | A.Var v -> I.Seq (I.Assign (var, I.Var v), cont)
  | A.Let (v, init, body) ->
      Hashtbl.add locals_types v init.ty;
      let cont = explicate_assign body var cont in
      explicate_assign init v cont

let explicate_def { A.name; params; retty; body } =
  Hashtbl.clear locals_types;
  List.iter (fun (name, ty) -> Hashtbl.add locals_types name ty) params;

  let start_label = genlabel () in
  let start_block = explicate_tail body in
  let blocks = (start_label, start_block) :: [] in
  let info =
    {
      Info.locals_types = Hashtbl.fold MapS.add locals_types MapS.empty;
      prelude_label = genlabel ();
      start_label;
      conclusion_label = genlabel ();
      stack_space = 0;
      cfg = MapS.empty;
      graph = Graph.graph_new ();
      used_callee = SetS.empty;
    }
  in
  { Ir.name; params; retty; blocks; info }

let run defs = List.map explicate_def defs
