open Utilities
open Ast

let rec collect_set { Ast.exp; ty = _ } =
  match exp with
  | Int _ -> SetS.empty
  | Read -> SetS.empty
  | Binop (_bop, exp1, exp2) -> collect_sets [ exp1; exp2 ]
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
  | Vector exps -> collect_sets exps
  | VectorLength exp1 -> collect_set exp1
  | VectorRef (exp1, _) -> collect_set exp1
  | VectorSet (exp1, _, exp2) -> collect_sets [ exp1; exp2 ]
  | Collect _ -> SetS.empty
  | Allocate _ -> SetS.empty
  | GlobalValue _ -> SetS.empty
  | Array (len, init) -> collect_sets [ len; init ]
  | ArrayLength exp1 -> collect_set exp1
  | ArrayRef (exp1, idx) -> collect_sets [ exp1; idx ]
  | ArraySet (exp1, idx, exp2) -> collect_sets [ exp1; idx; exp2 ]
  | Exit -> SetS.empty
  | AllocateArray (exp1, _) -> collect_set exp1
  | Apply (exp1, args) -> collect_sets (exp1 :: args)
  | FunRef _ -> SetS.empty
  | Lambda (_params, _retty, body) -> collect_set body
  | ProcedureArity e1 -> collect_set e1
  | Closure (_arity, es) -> collect_sets es
  | AllocateClosure (_len, _ty, _arity) -> SetS.empty

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
      | Binop (bop, e1, e2) -> Binop (bop, helper e1, helper e2)
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
      | Vector exps -> Vector (List.map helper exps)
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
      | Apply (e1, es) -> Apply (helper e1, List.map helper es)
      | FunRef (f, n) -> FunRef (f, n)
      | Lambda _ -> assert false
      | ProcedureArity e1 -> ProcedureArity (helper e1)
      | Closure _ -> assert false
      | AllocateClosure (len, ty, arity) -> AllocateClosure (len, ty, arity)
    in
    { Ast.exp; ty }
  in
  helper exp

let uncover_get_def { Ast.name; params; retty; body } =
  let set_vars = collect_set body in
  let body = uncover_get_exp set_vars body in
  { Ast.name; params; retty; body }

let run defs = List.map uncover_get_def defs
