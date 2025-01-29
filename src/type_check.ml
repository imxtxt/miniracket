open Utilities
module A = Ast
module T = Type

exception TypeError

let check_euqal t1 t2 = if T.(t1 = t2) then () else raise TypeError

let rec check_exp env { A.exp; _ } =
  match exp with
  | Int num -> { A.exp = A.Int num; ty = T.Integer }
  | Read -> { A.exp = A.Read; ty = T.Integer }
  | Add (e1, e2) -> (
      let e1 = check_exp env e1 in
      let e2 = check_exp env e2 in
      match (e1.ty, e2.ty) with
      | T.Integer, T.Integer -> { A.exp = A.Add (e1, e2); ty = T.Integer }
      | _ -> raise TypeError)
  | Sub (e1, e2) -> (
      let e1 = check_exp env e1 in
      let e2 = check_exp env e2 in
      match (e1.ty, e2.ty) with
      | T.Integer, T.Integer -> { A.exp = A.Sub (e1, e2); ty = T.Integer }
      | _ -> raise TypeError)
  | Mul (e1, e2) -> (
      let e1 = check_exp env e1 in
      let e2 = check_exp env e2 in
      match (e1.ty, e2.ty) with
      | T.Integer, T.Integer -> { A.exp = A.Mul (e1, e2); ty = T.Integer }
      | _ -> raise TypeError)
  | Var var -> { A.exp = A.Var var; ty = MapS.find var env }
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      let init = check_exp env init in
      let env = MapS.add var init.ty env in
      let body = check_exp env body in
      { A.exp = A.Let (var, init, body); ty = body.ty }
  | Bool b -> { A.exp = A.Bool b; ty = T.Boolean }
  | If (cnd, thn, els) ->
      let cnd = check_exp env cnd in
      let thn = check_exp env thn in
      let els = check_exp env els in
      check_euqal cnd.ty T.Boolean;
      check_euqal thn.ty els.ty;
      { A.exp = A.If (cnd, thn, els); ty = thn.ty }
  | Cmp (Eq, e1, e2) ->
      let e1 = check_exp env e1 in
      let e2 = check_exp env e2 in
      check_euqal e1.ty e2.ty;
      { A.exp = A.Cmp (Eq, e1, e2); ty = T.Boolean }
  | Cmp (cc, e1, e2) ->
      let e1 = check_exp env e1 in
      let e2 = check_exp env e2 in
      check_euqal e1.ty T.Integer;
      check_euqal e2.ty T.Integer;
      { A.exp = A.Cmp (cc, e1, e2); ty = T.Boolean }
  | Not e1 ->
      let e1 = check_exp env e1 in
      check_euqal e1.ty T.Boolean;
      { A.exp = A.Not e1; ty = T.Boolean }
  | SetBang (var, exp) ->
      let exp = check_exp env exp in
      let var_ty = MapS.find var env in
      check_euqal var_ty exp.ty;
      { A.exp = SetBang (var, exp); ty = T.Void }
  | Begin (exps, exp) ->
      let exps = List.map (check_exp env) exps in
      let exp = check_exp env exp in
      { A.exp = Begin (exps, exp); ty = exp.ty }
  | WhileLoop (cnd, body) ->
      let cnd = check_exp env cnd in
      let body = check_exp env body in
      check_euqal T.Boolean cnd.ty;
      { A.exp = WhileLoop (cnd, body); ty = T.Void }
  | Void -> { A.exp = Void; ty = T.Void }
  | Vector exps ->
      let exps = List.map (check_exp env) exps in
      let tys = List.map (fun { A.ty; _ } -> ty) exps in
      { A.exp = Vector exps; ty = T.Vector tys }
  | VectorLength exp -> (
      let exp = check_exp env exp in
      match exp.ty with
      | T.Vector _ -> { A.exp = VectorLength exp; ty = T.Integer }
      | _ -> raise TypeError)
  | VectorRef (exp, idx) -> (
      let exp = check_exp env exp in
      match exp.ty with
      | T.Vector tys -> { A.exp = VectorRef (exp, idx); ty = List.nth tys idx }
      | _ -> raise TypeError)
  | VectorSet (exp, idx, rhs) -> (
      let exp = check_exp env exp in
      let rhs = check_exp env rhs in
      match exp.ty with
      | T.Vector tys ->
          check_euqal (List.nth tys idx) rhs.ty;
          { A.exp = VectorSet (exp, idx, rhs); ty = T.Void }
      | _ -> raise TypeError)
  | Collect _ -> raise TypeError
  | Allocate _ -> raise TypeError
  | GlobalValue _ -> raise TypeError
  | Array (len, init) ->
      let len = check_exp env len in
      let init = check_exp env init in
      check_euqal len.ty T.Integer;
      { A.exp = Array (len, init); ty = T.Array init.ty }
  | ArrayLength exp -> (
      let exp = check_exp env exp in
      match exp.ty with
      | T.Array _ -> { A.exp = ArrayLength exp; ty = T.Integer }
      | _ -> raise TypeError)
  | ArrayRef (exp, idx) -> (
      let exp = check_exp env exp in
      let idx = check_exp env idx in
      match (exp.ty, idx.ty) with
      | T.Array ty, T.Integer -> { A.exp = ArrayRef (exp, idx); ty }
      | _ -> raise TypeError)
  | ArraySet (exp, idx, rhs) -> (
      let exp = check_exp env exp in
      let idx = check_exp env idx in
      let rhs = check_exp env rhs in
      match (exp.ty, idx.ty) with
      | T.Array ty, T.Integer ->
          check_euqal ty rhs.ty;
          { A.exp = ArraySet (exp, idx, rhs); ty = T.Void }
      | _ -> raise TypeError)
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (callee, args) -> (
      let callee = check_exp env callee in
      let args = List.map (check_exp env) args in
      match callee.ty with
      | T.Function (params, retty) ->
          if List.length params <> List.length args then raise TypeError
          else
            let _ = List.iter2 (fun p a -> check_euqal p a.A.ty) params args in
            { A.exp = Apply (callee, args); ty = retty }
      | _ -> raise TypeError)
  | FunRef _ -> assert false

let check_def env { A.name; params; retty; body } =
  let env =
    List.fold_left (fun acc (name, ty) -> MapS.add name ty acc) env params
  in
  let body = check_exp env body in
  if T.(body.ty = retty) then { A.name; params; retty; body }
  else raise TypeError

let run defs =
  let env =
    List.fold_left
      (fun env { Ast.name; params; retty; _ } ->
        let fty = T.Function (List.map snd params, retty) in
        MapS.add name fty env)
      MapS.empty defs
  in
  List.map (check_def env) defs
