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

let to_mon_bop (bop : A.bop) : M.bop =
  match bop with
  | Add -> Add
  | Sub -> Sub
  | Mul -> Mul

let rec rco_exp { A.exp; ty } =
  match exp with
  | A.Int num -> { M.exp = M.Int num; ty }
  | A.Read -> { M.exp = M.Read; ty }
  | Binop (bop, e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp =
        { M.exp = M.Binop (to_mon_bop bop, e1_atom, e2_atom); ty }
      in
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
  | A.Collect bytes ->
      let bytes_stmts, bytes_atom = rco_atom bytes in
      make_lets bytes_stmts { M.exp = M.Collect bytes_atom; ty }
  | A.Allocate (len, ty) -> { M.exp = M.Allocate (len, ty); ty }
  | A.GlobalValue _ -> assert false
  | A.Array _ -> assert false
  | A.ArrayLength e1 ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.ArrayLength e1_atom; ty } in
      make_lets e1_stmts new_exp
  | A.ArrayRef (e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.ArrayRef (e1_atom, e2_atom); ty } in
      make_lets (e1_stmts @ e2_stmts) new_exp
  | A.ArraySet (e1, idx, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let idx_stmts, idx_atom = rco_atom idx in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.ArraySet (e1_atom, idx_atom, e2_atom); ty } in
      make_lets (e1_stmts @ idx_stmts @ e2_stmts) new_exp
  | A.Exit -> { M.exp = M.Exit; ty }
  | A.AllocateArray (len, ty) ->
      let len_stmts, len_atom = rco_atom len in
      let new_exp = { M.exp = AllocateArray (len_atom, ty); ty } in
      make_lets len_stmts new_exp
  | A.Apply (callee, args) ->
      let callee_stmt, callee_atom = rco_atom callee in
      let args_stmts, args_atoms = List.map rco_atom args |> List.split in
      let args_stmts = List.flatten args_stmts in
      let new_exp = { M.exp = Apply (callee_atom, args_atoms); ty } in
      make_lets (callee_stmt @ args_stmts) new_exp
  | A.FunRef (f, arity) -> { M.exp = FunRef (f, arity); ty }
  | A.Lambda _ -> assert false
  | A.ProcedureArity e1 ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = ProcedureArity e1_atom; ty } in
      make_lets e1_stmts new_exp
  | A.Closure _ -> assert false
  | A.AllocateClosure (len, ty, arity) ->
      { M.exp = AllocateClosure (len, ty, arity); ty }

and rco_atom { A.exp; ty } =
  match exp with
  | A.Int num -> ([], M.Int num)
  | A.Read ->
      let tmp = gensym () in
      let exp = { M.exp = M.Read; ty } in
      ([ (tmp, exp) ], M.Var tmp)
  | Binop (bop, e1, e2) ->
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let tmp = gensym () in
      let new_exp = { M.exp = Binop (to_mon_bop bop, e1_atom, e2_atom); ty } in
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
  | A.Array _ -> assert false
  | A.ArrayLength e1 ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = M.ArrayLength e1_atom; ty } in
      (e1_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.ArrayRef (e1, e2) ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.ArrayRef (e1_atom, e2_atom); ty } in
      (e1_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.ArraySet (e1, idx, e2) ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let idx_stmts, idx_atom = rco_atom idx in
      let e2_stmts, e2_atom = rco_atom e2 in
      let new_exp = { M.exp = M.ArraySet (e1_atom, idx_atom, e2_atom); ty } in
      (e1_stmts @ idx_stmts @ e2_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | A.Exit -> assert false
  | A.AllocateArray _ -> assert false
  | Apply (callee, args) ->
      let callee_stmt, callee_atom = rco_atom callee in
      let args_stmts, args_atoms = List.map rco_atom args |> List.split in
      let args_stmts = List.flatten args_stmts in
      let new_exp = { M.exp = Apply (callee_atom, args_atoms); ty } in
      let tmp = gensym () in
      (callee_stmt @ args_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | FunRef (f, arity) ->
      let tmp = gensym () in
      let new_exp = { M.exp = FunRef (f, arity); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)
  | Lambda _ -> assert false
  | ProcedureArity e1 ->
      let tmp = gensym () in
      let e1_stmts, e1_atom = rco_atom e1 in
      let new_exp = { M.exp = ProcedureArity e1_atom; ty } in
      (e1_stmts @ [ (tmp, new_exp) ], M.Var tmp)
  | Closure _ -> assert false
  | AllocateClosure (len, ty, arity) ->
      let tmp = gensym () in
      let new_exp = { M.exp = AllocateClosure (len, ty, arity); ty } in
      ([ (tmp, new_exp) ], M.Var tmp)

let rco_def { A.name; params; retty; body } =
  let body = rco_exp body in
  { M.name; params; retty; body }

let run defs = List.map rco_def defs
