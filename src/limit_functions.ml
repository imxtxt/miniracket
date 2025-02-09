open Utilities
open Ast
open Type

let rec limit_type (ty : Type.ty) : Type.ty =
  match ty with
  | Integer -> Integer
  | Boolean -> Boolean
  | Void -> Void
  | Vector ts -> Vector (List.map limit_type ts)
  | Array ty -> Array (limit_type ty)
  | Function (tys, ty) ->
      let tys = List.map limit_type tys in
      let ty = limit_type ty in
      if List.length tys > 6 then
        let first_tys = Base.List.take tys 5 in
        let rest_tys = Base.List.drop tys 5 in
        let new_tys = first_tys @ [ Vector rest_tys ] in
        Function (new_tys, ty)
      else Function (tys, ty)
  | Dummy -> Dummy

let rec limit_exp env { Ast.exp; ty } =
  let exp =
    match exp with
    | Int i -> Int i
    | Read -> Read
    | Binop (bop, e1, e2) -> Binop (bop, limit_exp env e1, limit_exp env e2)
    | Var var -> (
        match MapS.find_opt var env with
        | Some (tup, idx) -> VectorRef (tup, idx)
        | None -> Var var)
    | GetBang _ -> assert false
    | Let (var, init, body) -> Let (var, limit_exp env init, limit_exp env body)
    | Bool b -> Bool b
    | If (e1, e2, e3) ->
        If (limit_exp env e1, limit_exp env e2, limit_exp env e3)
    | Cmp (cc, e1, e2) -> Cmp (cc, limit_exp env e1, limit_exp env e2)
    | Not e1 -> Not (limit_exp env e1)
    | SetBang (var, rhs) -> (
        let rhs = limit_exp env rhs in
        match MapS.find_opt var env with
        | Some (tup, idx) -> VectorSet (tup, idx, rhs)
        | None -> SetBang (var, rhs))
    | Begin (es, e) ->
        let es = List.map (limit_exp env) es in
        let e = limit_exp env e in
        Begin (es, e)
    | WhileLoop (e1, e2) -> WhileLoop (limit_exp env e1, limit_exp env e2)
    | Void -> Void
    | Vector es ->
        let es = List.map (limit_exp env) es in
        Vector es
    | VectorLength e1 -> VectorLength (limit_exp env e1)
    | VectorRef (e1, idx) -> VectorRef (limit_exp env e1, idx)
    | VectorSet (e1, idx, e2) ->
        VectorSet (limit_exp env e1, idx, limit_exp env e2)
    | Collect _ -> assert false
    | Allocate _ -> assert false
    | GlobalValue _ -> assert false
    | Array (e1, e2) -> Array (limit_exp env e1, limit_exp env e2)
    | ArrayLength e1 -> ArrayLength (limit_exp env e1)
    | ArrayRef (e1, e2) -> ArrayRef (limit_exp env e1, limit_exp env e2)
    | ArraySet (e1, e2, e3) ->
        ArraySet (limit_exp env e1, limit_exp env e2, limit_exp env e3)
    | Exit -> assert false
    | AllocateArray _ -> assert false
    | Apply (f, args) ->
        let f = limit_exp env f in
        let args = List.map (limit_exp env) args in
        if List.length args > 6 then
          let first_args = Base.List.take args 5 in
          let rest_args = Base.List.drop args 5 in
          let vector_ty =
            Type.Vector (List.map (fun e -> e.Ast.ty) rest_args)
          in
          let rest_arg = { Ast.exp = Vector rest_args; ty = vector_ty } in
          let new_args = first_args @ [ rest_arg ] in
          Apply (f, new_args)
        else Apply (f, args)
    | FunRef (f, arity) -> FunRef (f, arity)
    | Lambda _ -> assert false
    | ProcedureArity e1 -> ProcedureArity (limit_exp env e1)
    | Closure (arity, es) ->
        let es = List.map (limit_exp env) es in
        Closure (arity, es)
    | AllocateClosure _ -> assert false
  in
  { Ast.exp; ty = limit_type ty }

let limit_def { name; params; retty; body } =
  let params = List.map (fun (n, t) -> (n, limit_type t)) params in
  let retty = limit_type retty in
  if List.length params > 6 then
    let first_params = Base.List.take params 5 in
    let rest_params = Base.List.drop params 5 in
    let vector_ty = Type.Vector (List.map (fun (_, ty) -> ty) rest_params) in
    let vector_tmp = gensym () in
    let new_params = first_params @ [ (vector_tmp, vector_ty) ] in
    let vector_var = { Ast.exp = Var vector_tmp; ty = vector_ty } in
    let _, env =
      List.fold_left
        (fun (idx, env) (param, _) ->
          let env = MapS.add param (vector_var, idx) env in
          (idx + 1, env))
        (0, MapS.empty) rest_params
    in
    let body = limit_exp env body in
    { name; params = new_params; retty; body }
  else
    let body = limit_exp MapS.empty body in
    { name; params; retty; body }

let run defs = List.map limit_def defs
