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
  | Var var -> { A.exp = A.Var var; ty = MapS.find var env }
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
