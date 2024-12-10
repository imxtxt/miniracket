open Utilities
module A = Ast
module M = Ast_mon

let make_lets stmts acc =
  List.fold_right
    (fun (name, exp) acc -> { M.exp = M.Let (name, exp, acc); ty = acc.ty })
    stmts acc

let rec rco_exp { A.exp; ty } =
  match exp with
  | A.Int num -> { M.exp = M.Int num; ty }
  | A.Read -> { M.exp = M.Read; ty }
  | A.Add (e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.Add (e1_atom, e2_atom); ty } in
      make_lets (e1_stmts @ e2_stmts) new_exp
  | A.Sub (e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.Sub (e1_atom, e2_atom); ty } in
      make_lets (e1_stmts @ e2_stmts) new_exp
  | A.Var var -> { M.exp = M.Var var; ty }
  | A.Let (var, init, body) ->
      let init = rco_exp init in
      let body = rco_exp body in
      { M.exp = M.Let (var, init, body); ty }

and rco_atom { A.exp; ty } =
  match exp with
  | A.Int num -> ([], M.Int num)
  | A.Read ->
      let tmp = gensym () in
      let exp = { M.exp = M.Read; ty } in
      ([ (tmp, exp) ], M.Var tmp)
  | A.Add (e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let tmp = gensym () in
      let new_exp = { M.exp = M.Add (e1_atom, e2_atom); ty } in
      (e1_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.Sub (e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let tmp = gensym () in
      let new_exp = { M.exp = M.Sub (e1_atom, e2_atom); ty } in
      (e1_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.Var var -> ([], M.Var var)
  | A.Let (var, init, body) ->
      let init = rco_exp init in
      let body_stmts, body_atom = rco_atom body in
      ([ (var, init) ] @ body_stmts, body_atom)

let rco_def { A.name; params; retty; body } =
  let body = rco_exp body in
  { M.name; params; retty; body }

let run defs = List.map rco_def defs
