open Utilities
open Ast
module T = Type

let rec free_vars { Ast.exp; ty = _ } =
  match exp with
  | Int _
  | Read ->
      SetS.empty
  | Binop (_bop, e1, e2) -> free_vars_exps [ e1; e2 ]
  | Var var -> SetS.singleton var
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      let init_free_vars = free_vars init in
      let body_free_vars = free_vars body in
      SetS.union init_free_vars (SetS.remove var body_free_vars)
  | Bool _ -> SetS.empty
  | If (e1, e2, e3) -> free_vars_exps [ e1; e2; e3 ]
  | Cmp (_cc, e1, e2) -> free_vars_exps [ e1; e2 ]
  | Not e1 -> free_vars e1
  | SetBang (var, rhs) -> SetS.union (SetS.singleton var) (free_vars rhs)
  | Begin (es, e) -> free_vars_exps (e :: es)
  | WhileLoop (e1, e2) -> free_vars_exps [ e1; e2 ]
  | Void -> SetS.empty
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
  | FunRef (_f, _arity) -> SetS.empty
  | Lambda (params, _retty, body) ->
      let body_free_vars = free_vars body in
      List.fold_left
        (fun acc (param, _) -> SetS.remove param acc)
        body_free_vars params
  | ProcedureArity e1 -> free_vars e1
  | Closure _ -> assert false
  | AllocateClosure _ -> assert false

and free_vars_exps exps =
  List.fold_left (fun acc exp -> SetS.union acc (free_vars exp)) SetS.empty exps

let rec free_in_lambda { Ast.exp; ty } =
  match exp with
  | Int _
  | Read ->
      SetS.empty
  | Binop (_bop, e1, e2) -> free_in_lambda_exps [ e1; e2 ]
  | Var _ -> SetS.empty
  | GetBang _ -> assert false
  | Let (_var, init, body) -> free_in_lambda_exps [ init; body ]
  | Bool _ -> SetS.empty
  | If (e1, e2, e3) -> free_in_lambda_exps [ e1; e2; e3 ]
  | Cmp (_cc, e1, e2) -> free_in_lambda_exps [ e1; e2 ]
  | Not e1 -> free_in_lambda e1
  | SetBang (_var, rhs) -> free_in_lambda rhs
  | Begin (es, e) -> free_in_lambda_exps (e :: es)
  | WhileLoop (e1, e2) -> free_in_lambda_exps [ e1; e2 ]
  | Void -> SetS.empty
  | Vector es -> free_in_lambda_exps es
  | VectorLength e1
  | VectorRef (e1, _) ->
      free_in_lambda e1
  | VectorSet (e1, _idx, e2) -> free_in_lambda_exps [ e1; e2 ]
  | Collect _
  | Allocate _
  | GlobalValue _ ->
      assert false
  | Array (len, init) -> free_in_lambda_exps [ len; init ]
  | ArrayLength e1 -> free_in_lambda e1
  | ArrayRef (e1, idx) -> free_in_lambda_exps [ e1; idx ]
  | ArraySet (e1, idx, e2) -> free_in_lambda_exps [ e1; idx; e2 ]
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (e, es) -> free_in_lambda_exps (e :: es)
  | FunRef (_f, _arity) -> SetS.empty
  | Lambda (params, retty, body) ->
      let lam_free_vars =
        free_vars { Ast.exp = Lambda (params, retty, body); ty }
      in
      SetS.union lam_free_vars (free_in_lambda body)
  | ProcedureArity e1 -> free_in_lambda e1
  | Closure _ -> assert false
  | AllocateClosure _ -> assert false

and free_in_lambda_exps exps =
  List.fold_left
    (fun acc exp -> SetS.union acc (free_in_lambda exp))
    SetS.empty exps

let rec convert_exp af { Ast.exp; ty } =
  match exp with
  | Int i -> { Ast.exp = Int i; ty }
  | Read -> { Ast.exp = Read; ty }
  | Binop (bop, e1, e2) ->
      { Ast.exp = Binop (bop, convert_exp af e1, convert_exp af e2); ty }
  | Var var ->
      if SetS.mem var af then
        let vec_var = { Ast.exp = Var var; ty = T.Vector [ ty ] } in
        { Ast.exp = VectorRef (vec_var, 0); ty }
      else { Ast.exp = Var var; ty }
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      let init = convert_exp af init in
      if SetS.mem var af then
        let vec_init =
          { Ast.exp = Vector [ init ]; ty = T.Vector [ init.ty ] }
        in
        { Ast.exp = Let (var, vec_init, convert_exp af body); ty }
      else { Ast.exp = Let (var, init, convert_exp af body); ty }
  | Bool b -> { Ast.exp = Bool b; ty }
  | If (e1, e2, e3) ->
      let e1 = convert_exp af e1 in
      let e2 = convert_exp af e2 in
      let e3 = convert_exp af e3 in
      { Ast.exp = If (e1, e2, e3); ty }
  | Cmp (cc, e1, e2) ->
      { Ast.exp = Cmp (cc, convert_exp af e1, convert_exp af e2); ty }
  | Not e1 -> { Ast.exp = Not (convert_exp af e1); ty }
  | SetBang (var, rhs) ->
      let rhs = convert_exp af rhs in
      if SetS.mem var af then
        let vec_var = { Ast.exp = Var var; ty = T.Vector [ rhs.ty ] } in
        { Ast.exp = VectorSet (vec_var, 0, rhs); ty }
      else { Ast.exp = SetBang (var, rhs); ty }
  | Begin (es, e) ->
      let es = List.map (convert_exp af) es in
      let e = convert_exp af e in
      { Ast.exp = Begin (es, e); ty }
  | WhileLoop (e1, e2) ->
      { Ast.exp = WhileLoop (convert_exp af e1, convert_exp af e2); ty }
  | Void -> { Ast.exp = Void; ty }
  | Vector es ->
      let es = List.map (convert_exp af) es in
      { Ast.exp = Vector es; ty }
  | VectorLength e1 -> { Ast.exp = VectorLength (convert_exp af e1); ty }
  | VectorRef (e1, idx) -> { Ast.exp = VectorRef (convert_exp af e1, idx); ty }
  | VectorSet (e1, idx, e2) ->
      let e1 = convert_exp af e1 in
      let e2 = convert_exp af e2 in
      { Ast.exp = VectorSet (e1, idx, e2); ty }
  | Collect _
  | Allocate _
  | GlobalValue _ ->
      assert false
  | Array (len, init) ->
      { Ast.exp = Array (convert_exp af len, convert_exp af init); ty }
  | ArrayLength e1 -> { Ast.exp = ArrayLength (convert_exp af e1); ty }
  | ArrayRef (e1, idx) ->
      { Ast.exp = ArrayRef (convert_exp af e1, convert_exp af idx); ty }
  | ArraySet (e1, idx, e2) ->
      let e1 = convert_exp af e1 in
      let idx = convert_exp af idx in
      let e2 = convert_exp af e2 in
      { Ast.exp = ArraySet (e1, idx, e2); ty }
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (e, args) ->
      let e = convert_exp af e in
      let args = List.map (convert_exp af) args in
      { Ast.exp = Apply (e, args); ty }
  | FunRef (f, arity) -> { Ast.exp = FunRef (f, arity); ty }
  | Lambda (params, retty, body) ->
      let body = convert_exp af body in
      let new_params, body =
        List.fold_right
          (fun (param, ty) (new_params, body) ->
            if SetS.mem param af then
              let tmp = gensym () in
              let tmp_ast = { Ast.exp = Var tmp; ty } in
              let vec =
                { Ast.exp = Vector [ tmp_ast ]; ty = T.Vector [ ty ] }
              in
              let new_body =
                { Ast.exp = Let (param, vec, body); ty = body.ty }
              in
              (tmp :: new_params, new_body)
            else (param :: new_params, body))
          params ([], body)
      in
      let new_params = List.map2 (fun (_, ty) p -> (p, ty)) params new_params in
      { Ast.exp = Lambda (new_params, retty, body); ty }
  | ProcedureArity e1 -> { Ast.exp = ProcedureArity (convert_exp af e1); ty }
  | Closure _ -> assert false
  | AllocateClosure _ -> assert false

let convert_def { Ast.name; params; retty; body } =
  let af = SetS.inter (Uncover_get.collect_set body) (free_in_lambda body) in
  let body = convert_exp af body in
  let new_params, body =
    List.fold_right
      (fun (param, ty) (new_params, body) ->
        if SetS.mem param af then
          let tmp = gensym () in
          let tmp_ast = { Ast.exp = Var tmp; ty } in
          let vec = { Ast.exp = Vector [ tmp_ast ]; ty = T.Vector [ ty ] } in
          let new_body = { Ast.exp = Let (param, vec, body); ty = body.ty } in
          (tmp :: new_params, new_body)
        else (param :: new_params, body))
      params ([], body)
  in
  let new_params = List.map2 (fun (_, ty) p -> (p, ty)) params new_params in
  { Ast.name; params = new_params; retty; body }

let run defs = List.map convert_def defs
