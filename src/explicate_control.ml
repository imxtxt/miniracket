exception IrError

open Utilities
module A = Ast_mon
module I = Ir

let basic_blocks = ref []

let create_block tail =
  match tail with
  | I.Goto label -> label
  | _ ->
      let label = genlabel () in
      basic_blocks := (label, tail) :: !basic_blocks;
      label

let locals_types = Hashtbl.create 10

let to_ir_atom (atom : A.atom) : I.atom =
  match atom with
  | Int num -> Int num
  | Var var -> Var var
  | Bool b -> Bool b

let to_ir_cc (cc : A.cc) : I.cc =
  match cc with
  | Eq -> Eq
  | Lt -> Lt
  | Le -> Le
  | Gt -> Gt
  | Ge -> Ge

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
  | A.Bool b -> I.Return (I.Bool b)
  | A.If (cnd, thn, els) ->
      let thn_cont = explicate_tail thn in
      let els_cont = explicate_tail els in
      explicate_pred cnd thn_cont els_cont
  | A.Cmp (cc, a1, a2) ->
      I.Return (I.Cmp (to_ir_cc cc, to_ir_atom a1, to_ir_atom a2))
  | A.Not a1 -> I.Return (I.Not (to_ir_atom a1))

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
  | A.Bool b -> I.Seq (I.Assign (var, I.Bool b), cont)
  | A.If (cnd, thn, els) ->
      let join_label = create_block cont in
      let thn_cont = explicate_assign thn var (I.Goto join_label) in
      let els_cont = explicate_assign els var (I.Goto join_label) in
      explicate_pred cnd thn_cont els_cont
  | A.Cmp (cc, a1, a2) ->
      I.Seq
        (I.Assign (var, I.Cmp (to_ir_cc cc, to_ir_atom a1, to_ir_atom a2)), cont)
  | A.Not a1 -> I.Seq (I.Assign (var, I.Not (to_ir_atom a1)), cont)

and explicate_pred { A.exp; _ } thn_cont els_cont =
  match exp with
  | A.Int _
  | A.Read
  | A.Add _
  | A.Sub _ ->
      raise IrError
  | A.Var var ->
      let thn_label = create_block thn_cont in
      let els_label = create_block els_cont in
      I.IfStmt (I.Eq, I.Var var, I.Bool true, thn_label, els_label)
  | A.Let (var, init, body) ->
      Hashtbl.add locals_types var init.ty;
      let cont = explicate_pred body thn_cont els_cont in
      explicate_assign init var cont
  | A.Bool true -> thn_cont
  | A.Bool false -> els_cont
  | A.If (e1, e2, e3) ->
      let thn_label = create_block thn_cont in
      let els_label = create_block els_cont in
      let e2_cont = explicate_pred e2 (I.Goto thn_label) (I.Goto els_label) in
      let e3_cont = explicate_pred e3 (I.Goto thn_label) (I.Goto els_label) in
      explicate_pred e1 e2_cont e3_cont
  | A.Cmp (cc, a1, a2) ->
      let thn_label = create_block thn_cont in
      let els_label = create_block els_cont in
      I.IfStmt (to_ir_cc cc, to_ir_atom a1, to_ir_atom a2, thn_label, els_label)
  | A.Not (Int _) -> raise IrError
  | A.Not (A.Var var) ->
      let thn_label = create_block thn_cont in
      let els_label = create_block els_cont in
      I.IfStmt (I.Eq, I.Var var, I.Bool true, els_label, thn_label)
  | A.Not (A.Bool true) -> els_cont
  | A.Not (A.Bool false) -> thn_cont

let explicate_def { A.name; params; retty; body } =
  basic_blocks := [];
  Hashtbl.clear locals_types;
  List.iter (fun (name, ty) -> Hashtbl.add locals_types name ty) params;

  let start_label = genlabel () in
  let start_block = explicate_tail body in
  let blocks = (start_label, start_block) :: !basic_blocks in
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
