open Utilities
open Ast
module T = Type

let rec check_bounds_exp { Ast.exp; ty } =
  match exp with
  | Int i -> { Ast.exp = Int i; ty }
  | Read -> { Ast.exp = Read; ty }
  | Binop (bop, e1, e2) ->
      { exp = Binop (bop, check_bounds_exp e1, check_bounds_exp e2); ty }
  | Var var -> { exp = Var var; ty }
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      { exp = Let (var, check_bounds_exp init, check_bounds_exp body); ty }
  | Bool b -> { exp = Bool b; ty }
  | If (e1, e2, e3) ->
      let e1 = check_bounds_exp e1 in
      let e2 = check_bounds_exp e2 in
      let e3 = check_bounds_exp e3 in
      { exp = If (e1, e2, e3); ty }
  | Cmp (cc, e1, e2) ->
      { exp = Cmp (cc, check_bounds_exp e1, check_bounds_exp e2); ty }
  | Not e1 -> { exp = Not (check_bounds_exp e1); ty }
  | SetBang (var, rhs) -> { exp = SetBang (var, check_bounds_exp rhs); ty }
  | Begin (es, e) ->
      let es = List.map check_bounds_exp es in
      let e = check_bounds_exp e in
      { exp = Begin (es, e); ty }
  | WhileLoop (cnd, body) ->
      { exp = WhileLoop (check_bounds_exp cnd, check_bounds_exp body); ty }
  | Void -> { exp = Void; ty }
  | Vector es -> { exp = Vector (List.map check_bounds_exp es); ty }
  | VectorLength e -> { exp = VectorLength (check_bounds_exp e); ty }
  | VectorRef (e1, idx) -> { exp = VectorRef (check_bounds_exp e1, idx); ty }
  | VectorSet (e1, idx, e2) ->
      { exp = VectorSet (check_bounds_exp e1, idx, check_bounds_exp e2); ty }
  | Collect _ -> assert false
  | Allocate _ -> assert false
  | GlobalValue _ -> assert false
  | Array (len, init) ->
      { exp = Array (check_bounds_exp len, check_bounds_exp init); ty }
  | ArrayLength e1 -> { exp = ArrayLength (check_bounds_exp e1); ty }
  | ArrayRef (arr, idx) ->
      (*
        (array-ref arr idx) =>

        (let ([tmp1 arr])
          (let ([tmp2 idx])
            (if (< tmp2 (array-length tmp1))
                (if (>= tmp2 0)
                  (array-ref tmp1 tmp2)
                  (exit))
                (exit))))
      *)
      let arr = check_bounds_exp arr in
      let idx = check_bounds_exp idx in
      let tmp1 = gensym () in
      let tmp2 = gensym () in
      let tmp1_ast = { exp = Var tmp1; ty = arr.ty } in
      let tmp2_ast = { exp = Var tmp2; ty = idx.ty } in
      let arr_ref = { exp = ArrayRef (tmp1_ast, tmp2_ast); ty } in
      let exit_ast = { exp = Exit; ty } in
      let zero_ast = { exp = Int 0; ty = Type.Integer } in
      let cmp_ast = { exp = Cmp (Ge, tmp2_ast, zero_ast); ty = Type.Boolean } in
      let if_ast = { exp = If (cmp_ast, arr_ref, exit_ast); ty = arr_ref.ty } in
      let arr_len = { exp = ArrayLength tmp1_ast; ty = Type.Integer } in
      let cmp_ast = { exp = Cmp (Lt, tmp2_ast, arr_len); ty = Type.Boolean } in
      let if_ast = { exp = If (cmp_ast, if_ast, exit_ast); ty = if_ast.ty } in
      let acc = { exp = Let (tmp2, idx, if_ast); ty = if_ast.ty } in
      let acc = { exp = Let (tmp1, arr, acc); ty = acc.ty } in
      acc
  | ArraySet (arr, idx, rhs) ->
      (*
        (array-set! arr idx rhs) =>

        (let ([tmp1 arr])
          (let ([tmp2 idx])
            (let ([tmp3 rhs])
              (if (< tmp2 (array-length tmp1))
                  (if (>= tmp2 0)
                    (array-set! tmp1 tmp2 tmp3)
                    (exit))
                  (exit)))))
      *)
      let arr = check_bounds_exp arr in
      let idx = check_bounds_exp idx in
      let rhs = check_bounds_exp rhs in
      let tmp1 = gensym () in
      let tmp2 = gensym () in
      let tmp3 = gensym () in
      let tmp1_ast = { exp = Var tmp1; ty = arr.ty } in
      let tmp2_ast = { exp = Var tmp2; ty = idx.ty } in
      let tmp3_ast = { exp = Var tmp3; ty = rhs.ty } in
      let arr_set = { exp = ArraySet (tmp1_ast, tmp2_ast, tmp3_ast); ty } in
      let exit_ast = { exp = Exit; ty } in
      let zero_ast = { exp = Int 0; ty = Type.Integer } in
      let cmp_ast = { exp = Cmp (Ge, tmp2_ast, zero_ast); ty = Type.Boolean } in
      let if_ast = { exp = If (cmp_ast, arr_set, exit_ast); ty = arr_set.ty } in
      let arr_len = { exp = ArrayLength tmp1_ast; ty = Type.Integer } in
      let cmp_ast = { exp = Cmp (Lt, tmp2_ast, arr_len); ty = Type.Boolean } in
      let if_ast = { exp = If (cmp_ast, if_ast, exit_ast); ty = if_ast.ty } in
      let acc = { exp = Let (tmp3, rhs, if_ast); ty = if_ast.ty } in
      let acc = { exp = Let (tmp2, idx, acc); ty = acc.ty } in
      let acc = { exp = Let (tmp1, arr, acc); ty = acc.ty } in
      acc
  | Exit -> assert false
  | AllocateArray _ -> assert false
  | Apply (callee, args) ->
      let callee = check_bounds_exp callee in
      let args = List.map check_bounds_exp args in
      { exp = Apply (callee, args); ty }
  | FunRef (f, arity) -> { exp = FunRef (f, arity); ty }
  | Lambda _ -> assert false
  | ProcedureArity e1 -> { exp = ProcedureArity (check_bounds_exp e1); ty }
  | Closure (arity, es) ->
      let es = List.map check_bounds_exp es in
      { exp = Closure (arity, es); ty }
  | AllocateClosure _ -> assert false

let check_bounds_def (def : Ast.def) =
  let body = check_bounds_exp def.body in
  { def with body }

let run defs = List.map check_bounds_def defs
