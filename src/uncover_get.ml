open Utilities
open Ast

let rec collect_set { Ast.exp; ty = _ } =
  match exp with
  | Int _ -> SetS.empty
  | Read -> SetS.empty
  | Add (exp1, exp2) -> collect_sets [ exp1; exp2 ]
  | Sub (exp1, exp2) -> collect_sets [ exp1; exp2 ]
  | Var _ -> SetS.empty
  | GetBang _ -> assert false
  | Let (_, init, body) -> collect_sets [ init; body ]
  | Bool _ -> SetS.empty
  | If (exp1, exp2, exp3) -> collect_sets [ exp1; exp2; exp3 ]
  | Cmp (_, exp1, exp2) -> collect_sets [ exp1; exp2 ]
  | Not exp1 -> collect_set exp1
  | SetBang (var, exp) -> SetS.union (SetS.singleton var) (collect_set exp)
  | Begin (exps, exp) -> collect_sets (exp :: exps)
  | WhileLoop (cnd, body) -> collect_sets [ cnd; body ]
  | Void -> SetS.empty

and collect_sets exps =
  List.fold_left
    (fun acc exp -> SetS.union acc (collect_set exp))
    SetS.empty exps

let uncover_get_exp set_vars exp =
  let rec helper { Ast.exp; ty } =
    let exp =
      match exp with
      | Int i -> Int i
      | Read -> Read
      | Add (e1, e2) -> Add (helper e1, helper e2)
      | Sub (e1, e2) -> Sub (helper e1, helper e2)
      | Var var -> if SetS.mem var set_vars then GetBang var else Var var
      | GetBang _ -> assert false
      | Let (var, init, body) -> Let (var, helper init, helper body)
      | Bool b -> Bool b
      | If (e1, e2, e3) -> If (helper e1, helper e2, helper e3)
      | Cmp (cc, e1, e2) -> Cmp (cc, helper e1, helper e2)
      | Not e1 -> Not (helper e1)
      | SetBang (var, exp) -> SetBang (var, helper exp)
      | Begin (exps, exp) -> Begin (List.map helper exps, helper exp)
      | WhileLoop (cnd, body) -> WhileLoop (helper cnd, helper body)
      | Void -> Void
    in
    { Ast.exp; ty }
  in
  helper exp

let uncover_get_def { Ast.name; params; retty; body } =
  let set_vars = collect_set body in
  let body = uncover_get_exp set_vars body in
  { Ast.name; params; retty; body }

let run defs = List.map uncover_get_def defs
