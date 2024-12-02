open Utilities

(* ======================= type check ======================= *)
module TypeCheck = struct
  module A = Ast
  module T = Type

  exception TypeError

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
end

(* ======================= uniquify ======================= *)
module Uniquify = struct
  open Ast

  let rec uni_exp env { Ast.exp; ty } =
    let new_exp =
      match exp with
      | Int num -> Int num
      | Read -> Read
      | Add (e1, e2) -> Add (uni_exp env e1, uni_exp env e2)
      | Sub (e1, e2) -> Sub (uni_exp env e1, uni_exp env e2)
      | Var var -> Var (MapS.find var env)
      | Let (var, init, body) ->
          let new_init = uni_exp env init in
          let new_var = gensym () in
          let new_env = MapS.add var new_var env in
          let new_body = uni_exp new_env body in
          Let (new_var, new_init, new_body)
    in
    { exp = new_exp; ty }

  let uni_def env { Ast.name; params; retty; body } =
    let env, params =
      List.fold_right
        (fun (name, ty) (env, params) ->
          let new_name = gensym () in
          let new_env = MapS.add name new_name env in
          let new_params = (new_name, ty) :: params in
          (new_env, new_params))
        params (env, [])
    in
    let body = uni_exp env body in
    { Ast.name; params; retty; body }

  let run defs =
    let env =
      List.fold_left
        (fun env { Ast.name; _ } -> MapS.add name name env)
        MapS.empty defs
    in
    List.map (uni_def env) defs
end

(* ======================= remove complex operands ======================= *)
module RemoveComplexOperands = struct
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
end

(* ======================= explicate control ======================= *)
module ExplicateControl = struct
  module A = Ast_mon
  module I = Ir

  let locals_types = Hashtbl.create 10

  let to_ir_atom (atom : A.atom) : I.atom =
    match atom with
    | Int num -> Int num
    | Var var -> Var var

  let rec explicate_tail { A.exp; _ } =
    match exp with
    | A.Int num -> I.Return (I.Int num)
    | A.Read -> I.Return I.Read
    | A.Add (a1, a2) -> I.Return (I.Add (to_ir_atom a1, to_ir_atom a2))
    | A.Sub (a1, a2) -> I.Return (I.Sub (to_ir_atom a1, to_ir_atom a2))
    | A.Var var -> I.Return (I.Var var)
    | A.Let (var, init, body) ->
        Hashtbl.add locals_types var init.ty;
        let cont = explicate_tail body in
        explicate_assign init var cont

  and explicate_assign { A.exp; _ } var cont =
    match exp with
    | A.Int num -> I.Seq (I.Assign (var, I.Int num), cont)
    | A.Read -> I.Seq (I.Assign (var, I.Read), cont)
    | A.Add (a1, a2) ->
        I.Seq (I.Assign (var, I.Add (to_ir_atom a1, to_ir_atom a2)), cont)
    | A.Sub (a1, a2) ->
        I.Seq (I.Assign (var, I.Sub (to_ir_atom a1, to_ir_atom a2)), cont)
    | A.Var v -> I.Seq (I.Assign (var, I.Var v), cont)
    | A.Let (v, init, body) ->
        Hashtbl.add locals_types v init.ty;
        let cont = explicate_assign body var cont in
        explicate_assign init v cont

  let explicate_def { A.name; params; retty; body } =
    Hashtbl.clear locals_types;
    List.iter (fun (name, ty) -> Hashtbl.add locals_types name ty) params;

    let start_label = genlabel () in
    let start_block = explicate_tail body in
    let blocks = (start_label, start_block) :: [] in
    let info =
      {
        Info.locals_types = Hashtbl.fold MapS.add locals_types MapS.empty;
        prelude_label = genlabel ();
        start_label;
        conclusion_label = genlabel ();
        stack_space = 0;
      }
    in
    { Ir.name; params; retty; blocks; info }

  let run defs = List.map explicate_def defs
end

(* ======================= locals types ======================= *)
module LocalsTypes = struct
  open Utilities
  open Ir

  let update_locals_types ({ Ir.info; _ } as def) =
    let defined_vars = DefinedVar.defined_vars_def def in
    let locals_types =
      MapS.filter (fun var _ -> SetS.mem var defined_vars) info.locals_types
    in
    let info = { info with locals_types } in
    { def with info }

  let run defs = List.map update_locals_types defs
end

(* ======================= select instructions ======================= *)
module SelectInstructions = struct
  open Ir
  open X86

  let select_atom (atom : Ir.atom) : X86.arg =
    match atom with
    | Int num -> Imm num
    | Var var -> Var var

  let select_assign dest (exp : Ir.exp) =
    match exp with
    | Int num ->
        let instr1 = Instr2 (Movq, Imm num, dest) in
        [ instr1 ]
    | Read ->
        let instr1 = Callq ("read_int", 0) in
        let instr2 = Instr2 (Movq, Reg "rax", dest) in
        [ instr1; instr2 ]
    | Add (atom1, atom2) ->
        let arg1 = select_atom atom1 in
        let arg2 = select_atom atom2 in
        let instr1 = Instr2 (Movq, arg1, dest) in
        let instr2 = Instr2 (Addq, arg2, dest) in
        [ instr1; instr2 ]
    | Sub (atom1, atom2) ->
        let arg1 = select_atom atom1 in
        let arg2 = select_atom atom2 in
        let instr1 = Instr2 (Movq, arg1, dest) in
        let instr2 = Instr2 (Subq, arg2, dest) in
        [ instr1; instr2 ]
    | Var var ->
        let instr1 = Instr2 (Movq, Var var, dest) in
        [ instr1 ]

  let select_stmt (stmt : Ir.stmt) =
    match stmt with
    | Assign (var, rhs) -> select_assign (Var var) rhs

  let rec select_tail (info : Info.t) (tail : Ir.tail) =
    match tail with
    | Return exp ->
        select_assign (Reg "rax") exp @ [ Jmp info.conclusion_label ]
    | Seq (stmt, tail) -> select_stmt stmt @ select_tail info tail

  let select_def { Ir.name; params; blocks; retty = _; info } =
    let move_params =
      List.mapi
        (fun i (name, _) ->
          let reg = List.nth RegUse.arg_regs i in
          Instr2 (Movq, Reg reg, Var name))
        params
    in
    let x86_blocks =
      List.map
        (fun (lbl, tail) ->
          let tail_instrs = select_tail info tail in
          let instrs =
            if lbl = info.start_label then move_params @ tail_instrs
            else tail_instrs
          in
          (lbl, { instrs; liveafters = [] }))
        blocks
    in
    { X86.name; blocks = x86_blocks; info }

  let run defs = List.map select_def defs
end

(* ======================= assign home ======================= *)
module AssignHomes = struct
  open Utilities
  open X86

  let assign_home_arg env arg =
    match arg with
    | Imm imm -> Imm imm
    | Reg reg -> Reg reg
    | Var var -> MapS.find var env
    | Deref (idx, reg) -> Deref (idx, reg)

  let assign_home_instr env instr =
    match instr with
    | Instr2 (op2, arg1, arg2) ->
        let arg1 = assign_home_arg env arg1 in
        let arg2 = assign_home_arg env arg2 in
        Instr2 (op2, arg1, arg2)
    | Instr1 (op1, arg1) ->
        let arg1 = assign_home_arg env arg1 in
        Instr1 (op1, arg1)
    | Callq (lbl, arity) -> Callq (lbl, arity)
    | Retq -> Retq
    | Jmp lbl -> Jmp lbl

  let assign_home_block env { instrs; liveafters } =
    let instrs = List.map (assign_home_instr env) instrs in
    { instrs; liveafters }

  let assign_home_def { name; blocks; info } =
    let locals_types = info.locals_types in
    let stack_space = align_stack (MapS.cardinal locals_types * 8) in
    let _, env =
      MapS.fold
        (fun var _ (idx, env) ->
          let env = MapS.add var (Deref (idx, "rbp")) env in
          let idx = idx - 8 in
          (idx, env))
        locals_types (-8, MapS.empty)
    in
    let blocks =
      List.map (fun (lbl, blk) -> (lbl, assign_home_block env blk)) blocks
    in
    { name; blocks; info = { info with stack_space } }

  let run defs = List.map assign_home_def defs
end

(* ======================= patch instructions ======================= *)
module PatchInstructions = struct
  open X86

  let patch_instr instr =
    match instr with
    | Instr2 (op2, Deref (i1, r1), Deref (i2, r2)) ->
        let instr1 = Instr2 (Movq, Deref (i1, r1), Reg RegUse.patch_reg) in
        let instr2 = Instr2 (op2, Reg RegUse.patch_reg, Deref (i2, r2)) in
        [ instr1; instr2 ]
    | Instr2 (op2, Imm imm, Deref (i2, r2)) when imm > 65536 ->
        let instr1 = Instr2 (Movq, Imm imm, Reg RegUse.patch_reg) in
        let instr2 = Instr2 (op2, Reg RegUse.patch_reg, Deref (i2, r2)) in
        [ instr1; instr2 ]
    | Instr2 (op2, arg1, arg2) -> [ Instr2 (op2, arg1, arg2) ]
    | Instr1 (op1, arg1) -> [ Instr1 (op1, arg1) ]
    | Callq (lbl, arity) -> [ Callq (lbl, arity) ]
    | Retq -> [ Retq ]
    | Jmp lbl -> [ Jmp lbl ]

  let patch_block { instrs; liveafters } =
    let instrs = List.map patch_instr instrs |> List.flatten in
    { instrs; liveafters }

  let patch_def { name; blocks; info } =
    let blocks = List.map (fun (lbl, blk) -> (lbl, patch_block blk)) blocks in
    { name; blocks; info }

  let run defs = List.map patch_def defs
end

(* ======================= prelude conclusion ======================= *)
module PreludeConclusion = struct
  open X86

  let prelude_conclusion_def { name; blocks; info } =
    let prelude_instrs =
      [
        Instr1 (Pushq, Reg "rbp");
        Instr2 (Movq, Reg "rsp", Reg "rbp");
        Instr2 (Subq, Imm info.stack_space, Reg "rsp");
        Jmp info.start_label;
      ]
    in
    let prelude_block =
      (info.prelude_label, { instrs = prelude_instrs; liveafters = [] })
    in
    let conclusion_instrs =
      [
        Instr2 (Addq, Imm info.stack_space, Reg "rsp");
        Instr1 (Popq, Reg "rbp");
        Retq;
      ]
    in
    let conclusion_block =
      (info.conclusion_label, { instrs = conclusion_instrs; liveafters = [] })
    in
    { name; blocks = [ prelude_block ] @ blocks @ [ conclusion_block ]; info }

  let run defs = List.map prelude_conclusion_def defs
end
