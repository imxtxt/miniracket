open Utilities
open Type
open Ast

let is_atom { exp; ty = _ } =
  match exp with
  | Int _
  | Var _
  | Bool _
  | Void ->
      true
  | _ -> false

let rec expose_exp { Ast.exp; ty } =
  match exp with
  | Int num -> { Ast.exp = Int num; ty }
  | Read -> { Ast.exp = Read; ty }
  | Add (e1, e2) -> { Ast.exp = Add (expose_exp e1, expose_exp e2); ty }
  | Sub (e1, e2) -> { Ast.exp = Sub (expose_exp e1, expose_exp e2); ty }
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
      (* 
        (HasType (Prim 'vector (list 1 2)) (Vector Integer Integer)) =>

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
              match is_atom exp with
              | true -> exp
              | false -> { exp = Var var; ty = exp.ty }
            in
            { exp = VectorSet ({ exp = Var v; ty }, idx, rhs); ty = Void })
          bindings
      in
      let acc = { exp = Begin (vector_inits, acc); ty } in
      let acc = { exp = Let (v, { exp = Allocate (len, ty); ty }, acc); ty } in
      let free_ptr_ast = { exp = GlobalValue "free_ptr"; ty = Integer } in
      let bytes_ast = { exp = Int bytes; ty = Integer } in
      let add_ast = { exp = Add (free_ptr_ast, bytes_ast); ty = Integer } in
      let end_ast = { exp = GlobalValue "fromspace_end"; ty = Integer } in
      let cmp_ast = { exp = Cmp (Lt, add_ast, end_ast); ty = Boolean } in
      let void_ast = { exp = Void; ty = Void } in
      let collect_ast = { exp = Collect bytes; ty = Void } in
      let if_ast = { exp = If (cmp_ast, void_ast, collect_ast); ty = Void } in
      let acc = { exp = Begin ([ if_ast ], acc); ty = acc.ty } in
      List.fold_right
        (fun (var, exp) acc ->
          if is_atom exp then acc
          else { exp = Let (var, exp, acc); ty = acc.ty })
        bindings acc
  | VectorLength e1 -> { Ast.exp = VectorLength (expose_exp e1); ty }
  | VectorRef (e1, idx) -> { Ast.exp = VectorRef (expose_exp e1, idx); ty }
  | VectorSet (e1, idx, e2) ->
      { Ast.exp = VectorSet (expose_exp e1, idx, expose_exp e2); ty }
  | Collect _ -> assert false
  | Allocate _ -> assert false
  | GlobalValue _ -> assert false

let expose_def (def : Ast.def) =
  let body = expose_exp def.body in
  { def with body }

let run defs = List.map expose_def defs
