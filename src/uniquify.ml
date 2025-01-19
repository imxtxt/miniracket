open Utilities
open Ast

let rec uni_exp env { Ast.exp; ty } =
  let new_exp =
    match exp with
    | Int num -> Int num
    | Read -> Read
    | Add (e1, e2) -> Add (uni_exp env e1, uni_exp env e2)
    | Sub (e1, e2) -> Sub (uni_exp env e1, uni_exp env e2)
    | Var var -> Var (MapS.find var env)
    | GetBang _ -> assert false
    | Let (var, init, body) ->
        let new_init = uni_exp env init in
        let new_var = gensym () in
        let new_env = MapS.add var new_var env in
        let new_body = uni_exp new_env body in
        Let (new_var, new_init, new_body)
    | Bool b -> Bool b
    | If (e1, e2, e3) -> If (uni_exp env e1, uni_exp env e2, uni_exp env e3)
    | Cmp (cc, e1, e2) -> Cmp (cc, uni_exp env e1, uni_exp env e2)
    | Not e1 -> Not (uni_exp env e1)
    | SetBang (var, exp) -> SetBang (MapS.find var env, uni_exp env exp)
    | Begin (es, e) -> Begin (List.map (uni_exp env) es, uni_exp env e)
    | WhileLoop (cnd, body) -> WhileLoop (uni_exp env cnd, uni_exp env body)
    | Void -> Void
    | Vector es -> Vector (List.map (uni_exp env) es)
    | VectorLength e1 -> VectorLength (uni_exp env e1)
    | VectorRef (e1, idx) -> VectorRef (uni_exp env e1, idx)
    | VectorSet (e1, idx, e2) -> VectorSet (uni_exp env e1, idx, uni_exp env e2)
    | Collect _ -> assert false
    | Allocate _ -> assert false
    | GlobalValue _ -> assert false
  in
  { exp = new_exp; ty }

let uni_def env { Ast.name; params; retty; body } =
  let env, params =
    List.fold_right
      (fun (name, ty) (env, params) ->
        let new_name = gensym () in
        let new_env = MapS.add name new_name env in
        let new_params = (new_name, ty) :: params in
        (new_env, new_params))
      params (env, [])
  in
  let body = uni_exp env body in
  { Ast.name; params; retty; body }

let run defs =
  let env =
    List.fold_left
      (fun env { Ast.name; _ } -> MapS.add name name env)
      MapS.empty defs
  in
  List.map (uni_def env) defs
