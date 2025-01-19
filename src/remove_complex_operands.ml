open Utilities
module A = Ast
module M = Ast_mon

let make_lets stmts acc =
  List.fold_right
    (fun (name, exp) acc -> { M.exp = M.Let (name, exp, acc); ty = acc.ty })
    stmts acc

let to_mon_cc (cc : A.cc) : M.cc =
  match cc with
  | Eq -> Eq
  | Lt -> Lt
  | Le -> Le
  | Gt -> Gt
  | Ge -> Ge

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
  | A.GetBang var -> { M.exp = M.Var var; ty }
  | A.Let (var, init, body) ->
      let init = rco_exp init in
      let body = rco_exp body in
      { M.exp = M.Let (var, init, body); ty }
  | A.Bool b -> { M.exp = M.Bool b; ty }
  | A.If (e1, e2, e3) ->
      { M.exp = M.If (rco_exp e1, rco_exp e2, rco_exp e3); ty }
  | A.Cmp (cc, e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.Cmp (to_mon_cc cc, e1_atom, e2_atom); ty } in
      make_lets (e1_stmts @ e2_stmts) new_exp
  | A.Not e1 ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.Not e1_atom; ty } in
      make_lets e1_stmts new_exp
  | A.SetBang (var, exp) -> { M.exp = SetBang (var, rco_exp exp); ty }
  | A.Begin (exps, exp) ->
      let exps = List.map rco_exp exps in
      let exp = rco_exp exp in
      { M.exp = Begin (exps, exp); ty }
  | A.WhileLoop (cnd, body) ->
      { M.exp = WhileLoop (rco_exp cnd, rco_exp body); ty }
  | A.Void -> { M.exp = Void; ty }
  | A.Vector _ -> assert false
  | A.VectorLength e1 ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.VectorLength e1_atom; ty } in
      make_lets e1_stmts new_exp
  | VectorRef (e1, idx) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.VectorRef (e1_atom, idx); ty } in
      make_lets e1_stmts new_exp
  | VectorSet (e1, idx, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.VectorSet (e1_atom, idx, e2_atom); ty } in
      make_lets (e1_stmts @ e2_stmts) new_exp
  | A.Collect bytes -> { M.exp = M.Collect bytes; ty }
  | A.Allocate (len, ty) -> { M.exp = M.Allocate (len, ty); ty }
  | A.GlobalValue _ -> assert false

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
  | A.GetBang var ->
      let tmp = gensym () in
      let new_exp = { M.exp = M.Var var; ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | A.Let (var, init, body) ->
      let init = rco_exp init in
      let body_stmts, body_atom = rco_atom body in
      ([ (var, init) ] @ body_stmts, body_atom)
  | A.Bool b -> ([], M.Bool b)
  | A.If (e1, e2, e3) ->
      let tmp = gensym () in
      let new_exp = { M.exp = M.If (rco_exp e1, rco_exp e2, rco_exp e3); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | A.Cmp (cc, e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let tmp = gensym () in
      let new_exp = { M.exp = M.Cmp (to_mon_cc cc, e1_atom, e2_atom); ty } in
      (e1_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.Not e1 ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let tmp = gensym () in
      let new_exp = { M.exp = M.Not e1_atom; ty } in
      (e1_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.SetBang (var, exp) ->
      let tmp = gensym () in
      let new_exp = { M.exp = SetBang (var, rco_exp exp); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | A.Begin (exps, exp) ->
      let tmp = gensym () in
      let exps = List.map rco_exp exps in
      let exp = rco_exp exp in
      let new_exp = { M.exp = Begin (exps, exp); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | A.WhileLoop (cnd, body) ->
      let tmp = gensym () in
      let new_exp = { M.exp = WhileLoop (rco_exp cnd, rco_exp body); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | A.Void -> ([], M.Void)
  | A.Vector _ -> assert false
  | A.VectorLength e1 ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.VectorLength e1_atom; ty } in
      (e1_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.VectorRef (e1, idx) ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.VectorRef (e1_atom, idx); ty } in
      (e1_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.VectorSet (e1, idx, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let tmp = gensym () in
      let new_exp = { M.exp = M.VectorSet (e1_atom, idx, e2_atom); ty } in
      (e1_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.Collect _ -> assert false
  | A.Allocate _ -> assert false
  | A.GlobalValue label ->
      let tmp = gensym () in
      let new_exp = { M.exp = M.GlobalValue label; ty } in
      ([ (tmp, new_exp) ], M.Var tmp)

let rco_def { A.name; params; retty; body } =
  let body = rco_exp body in
  { M.name; params; retty; body }

let run defs = List.map rco_def defs
