open Utilities
open Ast
module T = Type

let top_funs = ref []

module Dict = struct
  type t = T.ty MapS.t

  let empty = MapS.empty
  let single k v = MapS.add k v MapS.empty

  let union (d1 : t) (d2 : t) =
    MapS.fold
      (fun k v acc -> if MapS.mem k acc then acc else MapS.add k v acc)
      d1 d2

  let remove (dict : t) (key : string) = MapS.remove key dict
  let to_list (dict : t) = MapS.bindings dict
end

let rec convert_type (ty : T.ty) : T.ty =
  match ty with
  | Integer -> Integer
  | Boolean -> Boolean
  | Void -> Void
  | Vector tys -> Vector (List.map convert_type tys)
  | Array ty -> Array (convert_type ty)
  | Function (param_tys, retty) ->
      let param_tys = List.map convert_type param_tys in
      let retty = convert_type retty in
      T.Vector [ T.Function (T.Vector [ T.Dummy ] :: param_tys, retty) ]
  | Dummy -> assert false

let rec free_vars { exp; ty } : Dict.t =
  match exp with
  | Int _
  | Read ->
      Dict.empty
  | Binop (_bop, e1, e2) -> free_vars_exps [ e1; e2 ]
  | Var var -> Dict.single var ty
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      let init_free_vars = free_vars init in
      let body_free_vars = free_vars body in
      Dict.union init_free_vars (Dict.remove body_free_vars var)
  | Bool _ -> Dict.empty
  | If (e1, e2, e3) -> free_vars_exps [ e1; e2; e3 ]
  | Cmp (_cc, e1, e2) -> free_vars_exps [ e1; e2 ]
  | Not e1 -> free_vars e1
  | SetBang (_var, rhs) -> free_vars rhs
  | Begin (es, e) -> free_vars_exps (e :: es)
  | WhileLoop (e1, e2) -> free_vars_exps [ e1; e2 ]
  | Void -> Dict.empty
  | Vector es -> free_vars_exps es
  | VectorLength e1
  | VectorRef (e1, _) ->
      free_vars e1
  | VectorSet (e1, _idx, e2) -> free_vars_exps [ e1; e2 ]
  | Collect _
  | Allocate _
  | GlobalValue _ ->
      assert false
  | Array (len, init) -> free_vars_exps [ len; init ]
  | ArrayLength e1 -> free_vars e1
  | ArrayRef (e1, idx) -> free_vars_exps [ e1; idx ]
  | ArraySet (e1, idx, e2) -> free_vars_exps [ e1; idx; e2 ]
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (e, es) -> free_vars_exps (e :: es)
  | FunRef (_f, _arity) -> Dict.empty
  | Lambda (params, _retty, body) ->
      let body_free_vars = free_vars body in
      List.fold_left
        (fun acc (param, _) -> Dict.remove acc param)
        body_free_vars params
  | ProcedureArity e1 -> free_vars e1
  | Closure _ -> assert false
  | AllocateClosure _ -> assert false

and free_vars_exps exps =
  List.fold_left (fun acc exp -> Dict.union acc (free_vars exp)) Dict.empty exps

let rec to_clo { exp; ty } =
  let to_fun_ty ty =
    match ty with
    | T.Vector (fun_ty :: _) -> fun_ty
    | _ -> assert false
  in
  match exp with
  | Int num -> { exp = Int num; ty }
  | Read -> { exp = Read; ty }
  | Binop (bop, e1, e2) -> { exp = Binop (bop, to_clo e1, to_clo e2); ty }
  | Var var -> { exp = Var var; ty = convert_type ty }
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      { exp = Let (var, to_clo init, to_clo body); ty = convert_type ty }
  | Bool bool -> { exp = Bool bool; ty }
  | If (e1, e2, e3) ->
      { exp = If (to_clo e1, to_clo e2, to_clo e3); ty = convert_type ty }
  | Cmp (cc, e1, e2) -> { exp = Cmp (cc, to_clo e1, to_clo e2); ty }
  | Not e1 -> { exp = Not (to_clo e1); ty }
  | SetBang (var, exp) -> { exp = SetBang (var, to_clo exp); ty }
  | Begin (es, e) ->
      { exp = Begin (List.map to_clo es, to_clo e); ty = convert_type ty }
  | WhileLoop (cnd, body) -> { exp = WhileLoop (to_clo cnd, to_clo body); ty }
  | Void -> { exp = Void; ty }
  | Vector es -> { exp = Vector (List.map to_clo es); ty = convert_type ty }
  | VectorLength e1 -> { exp = VectorLength (to_clo e1); ty }
  | VectorRef (e1, i) ->
      { exp = VectorRef (to_clo e1, i); ty = convert_type ty }
  | VectorSet (e1, i, e2) -> { exp = VectorSet (to_clo e1, i, to_clo e2); ty }
  | Collect _
  | Allocate _
  | GlobalValue _ ->
      assert false
  | Array (e1, e2) ->
      { exp = Array (to_clo e1, to_clo e2); ty = convert_type ty }
  | ArrayLength e1 -> { exp = ArrayLength (to_clo e1); ty }
  | ArrayRef (e1, i) ->
      { exp = ArrayRef (to_clo e1, to_clo i); ty = convert_type ty }
  | ArraySet (e1, i, e2) ->
      { exp = ArraySet (to_clo e1, to_clo i, to_clo e2); ty }
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (e, es) ->
      let e = to_clo e in
      let es = List.map to_clo es in
      let tmp = gensym () in
      let tmp_ast = { exp = Var tmp; ty = e.ty } in
      let vec_ref = { exp = VectorRef (tmp_ast, 0); ty = to_fun_ty e.ty } in
      let new_exp =
        { exp = Apply (vec_ref, tmp_ast :: es); ty = convert_type ty }
      in
      { exp = Let (tmp, e, new_exp); ty = new_exp.ty }
  | FunRef (f, arity) ->
      let ty = convert_type ty in
      let fun_ref = { exp = FunRef (f, arity); ty = to_fun_ty ty } in
      { exp = Closure (arity, [ fun_ref ]); ty }
  | Lambda (params, retty, body) ->
      (* create a top-level function *)
      let free_vars =
        free_vars { Ast.exp; ty }
        |> Dict.to_list
        |> List.map (fun (fv, ty) -> (fv, convert_type ty))
      in
      let free_vars_tys = List.map snd free_vars in
      let clo_param = gensym () in
      let clo_param_ty = T.Vector (T.Dummy :: free_vars_tys) in
      let clo_var = { exp = Var clo_param; ty = clo_param_ty } in
      let _, body =
        List.fold_right
          (fun (fv, ty) (idx, body) ->
            let vec_ref = { exp = VectorRef (clo_var, idx); ty } in
            let body = { exp = Let (fv, vec_ref, body); ty = body.ty } in
            (idx - 1, body))
          free_vars
          (List.length free_vars, to_clo body)
      in
      let top_fun_name = gensym () in
      let params = List.map (fun (p, ty) -> (p, convert_type ty)) params in
      let top_fun =
        {
          name = top_fun_name;
          params = (clo_param, clo_param_ty) :: params;
          retty = convert_type retty;
          body;
        }
      in
      top_funs := top_fun :: !top_funs;

      (* create a closure *)
      let arity = List.length params in
      let top_fun_ty =
        T.Function (List.map snd top_fun.params, top_fun.retty)
      in
      let top_fun_ref =
        { exp = FunRef (top_fun_name, arity); ty = top_fun_ty }
      in
      let free_vars_exps =
        List.map (fun (fv, ty) -> { exp = Var fv; ty }) free_vars
      in
      {
        exp = Closure (arity, top_fun_ref :: free_vars_exps);
        ty = T.Vector (top_fun_ref.ty :: free_vars_tys);
      }
  | ProcedureArity e1 -> { exp = ProcedureArity (to_clo e1); ty }
  | Closure _ -> assert false
  | AllocateClosure _ -> assert false

let to_clo_def { Ast.name; params; retty; body } =
  let params = List.map (fun (param, ty) -> (param, convert_type ty)) params in
  let closure_param = (gensym (), T.Vector [ T.Dummy ]) in
  {
    Ast.name;
    params = closure_param :: params;
    retty = convert_type retty;
    body = to_clo body;
  }

let run defs =
  top_funs := [];
  let defs = List.map to_clo_def defs in
  !top_funs @ defs
