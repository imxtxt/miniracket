open Utilities
open Ast

let rec collect_set { Ast.exp; ty = _ } =
  match exp with
  | Int _ -> SetS.empty
  | Read -> SetS.empty
  | Add (exp1, exp2) -> collect_sets [ exp1; exp2 ]
  | Sub (exp1, exp2) -> collect_sets [ exp1; exp2 ]
  | Mul (exp1, exp2) -> collect_sets [ exp1; exp2 ]
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
  | Vector _ -> assert false
  | VectorLength exp1 -> collect_set exp1
  | VectorRef (exp1, _) -> collect_set exp1
  | VectorSet (exp1, _, exp2) -> collect_sets [ exp1; exp2 ]
  | Collect _ -> SetS.empty
  | Allocate _ -> SetS.empty
  | GlobalValue _ -> SetS.empty
  | Array _ -> assert false
  | ArrayLength exp1 -> collect_set exp1
  | ArrayRef (exp1, idx) -> collect_sets [ exp1; idx ]
  | ArraySet (exp1, idx, exp2) -> collect_sets [ exp1; idx; exp2 ]
  | Exit -> SetS.empty
  | AllocateArray (exp1, _) -> collect_set exp1

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
      | Mul (e1, e2) -> Mul (helper e1, helper e2)
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
      | Vector _ -> assert false
      | VectorLength e1 -> VectorLength (helper e1)
      | VectorRef (e1, idx) -> VectorRef (helper e1, idx)
      | VectorSet (e1, idx, e2) -> VectorSet (helper e1, idx, helper e2)
      | Collect bytes -> Collect bytes
      | Allocate (len, ty) -> Allocate (len, ty)
      | GlobalValue label -> GlobalValue label
      | Array _ -> assert false
      | ArrayLength e1 -> ArrayLength (helper e1)
      | ArrayRef (e1, idx) -> ArrayRef (helper e1, helper idx)
      | ArraySet (e1, idx, e2) -> ArraySet (helper e1, helper idx, helper e2)
      | Exit -> Exit
      | AllocateArray (e1, ty) -> AllocateArray (helper e1, ty)
    in
    { Ast.exp; ty }
  in
  helper exp

let uncover_get_def { Ast.name; params; retty; body } =
  let set_vars = collect_set body in
  let body = uncover_get_exp set_vars body in
  { Ast.name; params; retty; body }

let run defs = List.map uncover_get_def defs
