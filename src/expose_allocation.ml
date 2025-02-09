open Utilities
open Type
open Ast

let is_pure { exp; ty = _ } =
  match exp with
  | Int _
  | Bool _
  | Void ->
      true
  | _ -> false

let rec expose_exp { Ast.exp; ty } =
  match exp with
  | Int num -> { Ast.exp = Int num; ty }
  | Read -> { Ast.exp = Read; ty }
  | Binop (bop, e1, e2) ->
      { Ast.exp = Binop (bop, expose_exp e1, expose_exp e2); ty }
  | Var var -> { Ast.exp = Var var; ty }
  | GetBang _ -> assert false
  | Let (var, init, body) ->
      { Ast.exp = Let (var, expose_exp init, expose_exp body); ty }
  | Bool b -> { Ast.exp = Bool b; ty }
  | If (e1, e2, e3) ->
      { Ast.exp = If (expose_exp e1, expose_exp e2, expose_exp e3); ty }
  | Cmp (cc, e1, e2) -> { Ast.exp = Cmp (cc, expose_exp e1, expose_exp e2); ty }
  | Not e1 -> { Ast.exp = Not (expose_exp e1); ty }
  | SetBang (var, e1) -> { Ast.exp = SetBang (var, expose_exp e1); ty }
  | Begin (es, e) ->
      { Ast.exp = Begin (List.map expose_exp es, expose_exp e); ty }
  | WhileLoop (cnd, body) ->
      { Ast.exp = WhileLoop (expose_exp cnd, expose_exp body); ty }
  | Void -> { Ast.exp = Void; ty }
  | Vector es ->
      allocate_tuple { exp = Allocate (List.length es, ty); ty } es ty
  | VectorLength e1 -> { Ast.exp = VectorLength (expose_exp e1); ty }
  | VectorRef (e1, idx) -> { Ast.exp = VectorRef (expose_exp e1, idx); ty }
  | VectorSet (e1, idx, e2) ->
      { Ast.exp = VectorSet (expose_exp e1, idx, expose_exp e2); ty }
  | Collect _ -> assert false
  | Allocate _ -> assert false
  | GlobalValue _ -> assert false
  | Array (len_exp, init_exp) ->
      (* 
        (let ([len len_exp])
          (let ([init init_exp])
            (let ([bytes (+ 8 (mul 8 len))])
              (begin
                (if (< (+ bytes free_ptr) fromspace_end)
                    (void)
                    (collect bytes))
                (let (arr (allocate-array len ty))
                  (let ([idx 0])
                    (begin
                      (while (< idx len)
                        (begin
                          (array-set! arr idx init)
                          (set! idx (+ idx 1))))
                      arr)))))))
      *)
      let len_exp = expose_exp len_exp in
      let init_exp = expose_exp init_exp in
      let len = gensym () in
      let init = gensym () in
      let bytes = gensym () in
      let arr = gensym () in
      let idx = gensym () in
      let len_ast = { exp = Var len; ty = len_exp.ty } in
      let init_ast = { exp = Var init; ty = init_exp.ty } in
      let bytes_ast = { exp = Var bytes; ty = Integer } in
      let arr_ast = { exp = Var arr; ty } in
      let idx_ast = { exp = Var idx; ty = Integer } in
      let aset = { exp = ArraySet (arr_ast, idx_ast, init_ast); ty = Void } in
      let one_ast = { exp = Int 1; ty = Integer } in
      let incr_idx = { exp = Binop (Add, idx_ast, one_ast); ty = Integer } in
      let idx_set = { exp = SetBang (idx, incr_idx); ty = Void } in
      let while_cmp = { exp = Cmp (Lt, idx_ast, len_ast); ty = Boolean } in
      let while_body = { exp = Begin ([ aset ], idx_set); ty = Void } in
      let while_exp = { exp = WhileLoop (while_cmp, while_body); ty = Void } in
      let acc = { exp = Begin ([ while_exp ], arr_ast); ty = arr_ast.ty } in
      let zero_ast = { exp = Int 0; ty = Integer } in
      let acc = { exp = Let (idx, zero_ast, acc); ty = acc.ty } in
      let allocate_array = { exp = AllocateArray (len_ast, ty); ty } in
      let acc = { exp = Let (arr, allocate_array, acc); ty = acc.ty } in
      let free_ptr_ast = { exp = GlobalValue "free_ptr"; ty = Integer } in
      let add_ast =
        { exp = Binop (Add, free_ptr_ast, bytes_ast); ty = Integer }
      in
      let end_ast = { exp = GlobalValue "fromspace_end"; ty = Integer } in
      let cmp_ast = { exp = Cmp (Lt, add_ast, end_ast); ty = Boolean } in
      let void_ast = { exp = Void; ty = Void } in
      let collect_ast = { exp = Collect bytes_ast; ty = Void } in
      let if_ast = { exp = If (cmp_ast, void_ast, collect_ast); ty = Void } in
      let acc = { exp = Begin ([ if_ast ], acc); ty = acc.ty } in
      let eight_ast = { exp = Int 8; ty = Integer } in
      let len_bytes = { exp = Binop (Mul, len_ast, eight_ast); ty = Integer } in
      let bytes_rhs =
        { exp = Binop (Add, eight_ast, len_bytes); ty = Integer }
      in
      let acc = { exp = Let (bytes, bytes_rhs, acc); ty = acc.ty } in
      let acc = { exp = Let (init, init_exp, acc); ty = acc.ty } in
      let acc = { exp = Let (len, len_exp, acc); ty = acc.ty } in
      acc
  | ArrayLength e1 -> { Ast.exp = ArrayLength (expose_exp e1); ty }
  | ArrayRef (e1, idx) ->
      { Ast.exp = ArrayRef (expose_exp e1, expose_exp idx); ty }
  | ArraySet (e1, idx, e2) ->
      { Ast.exp = ArraySet (expose_exp e1, expose_exp idx, expose_exp e2); ty }
  | Exit -> { Ast.exp = Exit; ty }
  | AllocateArray _ -> assert false
  | Apply (callee, args) ->
      let callee = expose_exp callee in
      let args = List.map expose_exp args in
      { exp = Apply (callee, args); ty }
  | FunRef (f, arity) -> { exp = FunRef (f, arity); ty }
  | Lambda _ -> assert false
  | ProcedureArity e -> { exp = ProcedureArity (expose_exp e); ty }
  | Closure (arity, es) ->
      let len = List.length es in
      let allocate = { exp = AllocateClosure (len, ty, arity); ty } in
      allocate_tuple allocate es ty
  | AllocateClosure _ -> assert false

and allocate_tuple allocate es ty =
  (* 
        (let ([tmp1 1])
          (let ([tmp2 2]))
            (begin 
               (if (< (+ free_ptr bytes) fromspace_end))
                   (void)
                   (collect bytes))
               (let ([v (allocate len ty)])
                 (begin
                   (vector-set! v 0 tmp1)
                   (vector-set! v 1 tmp2)
                   v)))
      *)
  let len = List.length es in
  let bytes = (len + 1) * 8 in
  let bindings = List.map (fun exp -> (gensym (), expose_exp exp)) es in
  let v = gensym () in
  let acc = { exp = Var v; ty } in
  let vector_inits =
    List.mapi
      (fun idx (var, exp) ->
        let rhs =
          match is_pure exp with
          | true -> exp
          | false -> { exp = Var var; ty = exp.ty }
        in
        { exp = VectorSet ({ exp = Var v; ty }, idx, rhs); ty = Void })
      bindings
  in
  let acc = { exp = Begin (vector_inits, acc); ty } in
  let acc = { exp = Let (v, allocate, acc); ty } in
  let free_ptr_ast = { exp = GlobalValue "free_ptr"; ty = Integer } in
  let bytes_ast = { exp = Int bytes; ty = Integer } in
  let add_ast = { exp = Binop (Add, free_ptr_ast, bytes_ast); ty = Integer } in
  let end_ast = { exp = GlobalValue "fromspace_end"; ty = Integer } in
  let cmp_ast = { exp = Cmp (Lt, add_ast, end_ast); ty = Boolean } in
  let void_ast = { exp = Void; ty = Void } in
  let collect_ast = { exp = Collect bytes_ast; ty = Void } in
  let if_ast = { exp = If (cmp_ast, void_ast, collect_ast); ty = Void } in
  let acc = { exp = Begin ([ if_ast ], acc); ty = acc.ty } in
  List.fold_right
    (fun (var, exp) acc ->
      if is_pure exp then acc else { exp = Let (var, exp, acc); ty = acc.ty })
    bindings acc

let expose_def (def : Ast.def) =
  let body = expose_exp def.body in
  { def with body }

let run defs = List.map expose_def defs
