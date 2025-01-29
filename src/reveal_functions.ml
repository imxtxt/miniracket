open Utilities
open Ast

let rec reveal_exp env { Ast.exp; ty } =
  let exp =
    match exp with
    | Int i -> Int i
    | Read -> Read
    | Add (e1, e2) -> Add (reveal_exp env e1, reveal_exp env e2)
    | Sub (e1, e2) -> Sub (reveal_exp env e1, reveal_exp env e2)
    | Mul (e1, e2) -> Mul (reveal_exp env e1, reveal_exp env e2)
    | Var var -> (
        match MapS.find_opt var env with
        | Some arity -> FunRef (var, arity)
        | None -> Var var)
    | GetBang _ -> assert false
    | Let (var, init, body) ->
        Let (var, reveal_exp env init, reveal_exp env body)
    | Bool b -> Bool b
    | If (e1, e2, e3) ->
        If (reveal_exp env e1, reveal_exp env e2, reveal_exp env e3)
    | Cmp (cc, e1, e2) -> Cmp (cc, reveal_exp env e1, reveal_exp env e2)
    | Not e1 -> Not (reveal_exp env e1)
    | SetBang (var, exp) -> SetBang (var, reveal_exp env exp)
    | Begin (es, e) ->
        let es = List.map (reveal_exp env) es in
        let e = reveal_exp env e in
        Begin (es, e)
    | WhileLoop (e1, e2) -> WhileLoop (reveal_exp env e1, reveal_exp env e2)
    | Void -> Void
    | Vector es -> Vector (List.map (reveal_exp env) es)
    | VectorLength e -> VectorLength (reveal_exp env e)
    | VectorRef (e, i) -> VectorRef (reveal_exp env e, i)
    | VectorSet (e1, i, e2) ->
        VectorSet (reveal_exp env e1, i, reveal_exp env e2)
    | Collect _ -> assert false
    | Allocate _ -> assert false
    | GlobalValue _ -> assert false
    | Array (len, init) -> Array (reveal_exp env len, reveal_exp env init)
    | ArrayLength e -> ArrayLength (reveal_exp env e)
    | ArrayRef (e1, e2) -> ArrayRef (reveal_exp env e1, reveal_exp env e2)
    | ArraySet (e1, e2, e3) ->
        ArraySet (reveal_exp env e1, reveal_exp env e2, reveal_exp env e3)
    | Exit -> assert false
    | AllocateArray _ -> assert false
    | Apply (callee, args) ->
        let callee = reveal_exp env callee in
        let args = List.map (reveal_exp env) args in
        Apply (callee, args)
    | FunRef _ -> assert false
  in
  { Ast.exp; ty }

let reveal_def env (def : Ast.def) =
  let body = reveal_exp env def.body in
  { def with body }

let run defs =
  let env =
    List.fold_left
      (fun acc { Ast.name; params; _ } ->
        MapS.add name (List.length params) acc)
      MapS.empty defs
  in
  List.map (reveal_def env) defs
