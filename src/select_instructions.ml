open Ir
open X86

let to_x86_cc (cc : Ir.cc) : X86.cc =
  match cc with
  | Eq -> E
  | Lt -> L
  | Le -> Le
  | Gt -> G
  | Ge -> Ge

let select_atom (atom : Ir.atom) : X86.arg =
  match atom with
  | Int num -> Imm num
  | Var var -> Var var
  | Bool true -> Imm 1
  | Bool false -> Imm 0
  | Void -> Imm 0

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
  | Mul (atom1, atom2) ->
      let arg1 = select_atom atom1 in
      let arg2 = select_atom atom2 in
      let instr1 = Instr2 (Movq, arg1, dest) in
      let instr2 = Instr2 (Imulq, arg2, dest) in
      [ instr1; instr2 ]
  | Var var ->
      let instr1 = Instr2 (Movq, Var var, dest) in
      [ instr1 ]
  | Bool true ->
      let instr1 = Instr2 (Movq, Imm 1, dest) in
      [ instr1 ]
  | Bool false ->
      let instr1 = Instr2 (Movq, Imm 0, dest) in
      [ instr1 ]
  | Cmp (cc, atom1, atom2) ->
      let arg1 = select_atom atom1 in
      let arg2 = select_atom atom2 in
      let instr1 = Instr2 (Cmpq, arg2, arg1) in
      let instr2 = Set (to_x86_cc cc, ByteReg "al") in
      let instr3 = Instr2 (Movzbq, ByteReg "al", dest) in
      [ instr1; instr2; instr3 ]
  | Not atom1 ->
      let arg1 = select_atom atom1 in
      let instr1 = Instr2 (Movq, arg1, dest) in
      let instr2 = Instr2 (Xorq, Imm 1, dest) in
      [ instr1; instr2 ]
  | Void ->
      let instr1 = Instr2 (Movq, Imm 0, dest) in
      [ instr1 ]
  | VectorLength atom1 ->
      let arg1 = select_atom atom1 in
      let instr1 = Instr2 (Movq, arg1, Reg "rax") in
      let instr2 = Load (0, "rax", "rax") in
      let instr3 = Instr2 (Sarq, Imm 1, Reg "rax") in
      let instr4 = Instr2 (Andq, Imm 0b111111, Reg "rax") in
      let instr5 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3; instr4; instr5 ]
  | VectorRef (atom1, idx) ->
      let arg1 = select_atom atom1 in
      let offset = (idx + 1) * 8 in
      let instr1 = Instr2 (Movq, arg1, Reg "rax") in
      let instr2 = Load (offset, "rax", "rax") in
      let instr3 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3 ]
  | VectorSet (atom1, idx, atom2) ->
      let arg1 = select_atom atom1 in
      let arg2 = select_atom atom2 in
      let offset = (idx + 1) * 8 in
      let instr1 = Instr2 (Movq, arg1, Reg "rax") in
      let instr2 = Instr2 (Movq, arg2, Reg "r10") in
      let instr3 = Store ("r10", offset, "rax") in
      let instr4 = Instr2 (Movq, Imm 0, dest) in
      [ instr1; instr2; instr3; instr4 ]
  | Allocate (len, ty) ->
      let compute_vector_tag = function
        | Type.Vector tys ->
            let tag =
              List.fold_left
                (fun acc ty ->
                  let acc = Int.shift_left acc 1 in
                  if Type.is_pointer ty then Int.logor acc 1 else acc)
                0 tys
            in
            let tag = Int.shift_left tag 6 in
            let tag = Int.logor tag (List.length tys) in
            let tag = Int.shift_left tag 1 in
            let tag = Int.logor tag 1 in
            tag
        | _ -> assert false
      in
      let tag = compute_vector_tag ty in
      let bytes = (len + 1) * 8 in
      let instr1 = Instr2 (Movq, Global "free_ptr", Reg "rax") in
      let instr2 = Instr2 (Addq, Imm bytes, Global "free_ptr") in
      let instr3 = Instr2 (Movq, Imm tag, Reg "r10") in
      let instr4 = Store ("r10", 0, "rax") in
      let instr5 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3; instr4; instr5 ]
  | GlobalValue label ->
      let instr1 = Instr2 (Movq, Global label, Reg "rax") in
      let instr2 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2 ]
  | ArrayLength atom1 ->
      let arg1 = select_atom atom1 in
      let instr1 = Instr2 (Movq, arg1, Reg "rax") in
      let instr2 = Load (0, "rax", "rax") in
      let instr3 = Instr2 (Salq, Imm 2, Reg "rax") in
      let instr4 = Instr2 (Shrq, Imm 4, Reg "rax") in
      let instr5 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3; instr4; instr5 ]
  | ArrayRef (arr, idx) ->
      let arr_arg = select_atom arr in
      let idx_arg = select_atom idx in
      let instr1 = Instr2 (Movq, arr_arg, Reg "rax") in
      let instr2 = Instr2 (Movq, idx_arg, Reg "r10") in
      let instr3 = Instr2 (Addq, Imm 1, Reg "r10") in
      let instr4 = Instr2 (Imulq, Imm 8, Reg "r10") in
      let instr5 = Instr2 (Addq, Reg "r10", Reg "rax") in
      let instr6 = Load (0, "rax", "rax") in
      let instr7 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3; instr4; instr5; instr6; instr7 ]
  | ArraySet (arr, idx, rhs) ->
      let arr_arg = select_atom arr in
      let idx_arg = select_atom idx in
      let rhs_arg = select_atom rhs in
      let instr1 = Instr2 (Movq, arr_arg, Reg "rax") in
      let instr2 = Instr2 (Movq, idx_arg, Reg "r10") in
      let instr3 = Instr2 (Addq, Imm 1, Reg "r10") in
      let instr4 = Instr2 (Imulq, Imm 8, Reg "r10") in
      let instr5 = Instr2 (Addq, Reg "r10", Reg "rax") in
      let instr6 = Instr2 (Movq, rhs_arg, Reg "r10") in
      let instr7 = Store ("r10", 0, "rax") in
      let instr8 = Instr2 (Movq, Imm 0, dest) in
      [ instr1; instr2; instr3; instr4; instr5; instr6; instr7; instr8 ]
  | AllocateArray (len, ty) ->
      let pointer_mask =
        match ty with
        | Type.Array ty -> if Type.is_pointer ty then 1 else 0
        | _ -> assert false
      in
      let len_arg = select_atom len in
      let instr1 = Instr2 (Movq, Imm 1, Reg "r10") in
      let instr2 = Instr2 (Salq, Imm 60, Reg "r10") in
      let instr3 = Instr2 (Orq, len_arg, Reg "r10") in
      let instr4 = Instr2 (Salq, Imm 1, Reg "r10") in
      let instr5 = Instr2 (Orq, Imm pointer_mask, Reg "r10") in
      let instr6 = Instr2 (Salq, Imm 1, Reg "r10") in
      let instr7 = Instr2 (Orq, Imm 1, Reg "r10") in
      let instr8 = Instr2 (Movq, Global "free_ptr", Reg "rax") in
      let instr9 = Store ("r10", 0, "rax") in
      let instr10 = Instr2 (Movq, len_arg, Reg "r10") in
      let instr11 = Instr2 (Addq, Imm 1, Reg "r10") in
      let instr12 = Instr2 (Imulq, Imm 8, Reg "r10") in
      let instr13 = Instr2 (Addq, Reg "r10", Global "free_ptr") in
      let instr14 = Instr2 (Movq, Reg "rax", dest) in
      [ instr1; instr2; instr3; instr4; instr5; instr6; instr7 ]
      @ [ instr8; instr9; instr10; instr11; instr12; instr13; instr14 ]

let select_stmt (stmt : Ir.stmt) =
  match stmt with
  | Assign (var, rhs) -> select_assign (Var var) rhs
  | ReadStmt ->
      let instr1 = Callq ("read_int", 0) in
      [ instr1 ]
  | VectorSetStmt (atom1, idx, atom2) ->
      let arg1 = select_atom atom1 in
      let arg2 = select_atom atom2 in
      let offset = (idx + 1) * 8 in
      let instr1 = Instr2 (Movq, arg1, Reg "rax") in
      let instr2 = Instr2 (Movq, arg2, Reg "r10") in
      let instr3 = Store ("r10", offset, "rax") in
      [ instr1; instr2; instr3 ]
  | Collect bytes ->
      let bytes_arg = select_atom bytes in
      let instr1 = Instr2 (Movq, Reg "r15", Reg "rdi") in
      let instr2 = Instr2 (Movq, bytes_arg, Reg "rsi") in
      let instr3 = Callq ("collect", 2) in
      [ instr1; instr2; instr3 ]
  | ArraySetStmt (arr, idx, rhs) ->
      let arr_arg = select_atom arr in
      let idx_arg = select_atom idx in
      let rhs_arg = select_atom rhs in
      let instr1 = Instr2 (Movq, arr_arg, Reg "rax") in
      let instr2 = Instr2 (Movq, idx_arg, Reg "r10") in
      let instr3 = Instr2 (Addq, Imm 1, Reg "r10") in
      let instr4 = Instr2 (Imulq, Imm 8, Reg "r10") in
      let instr5 = Instr2 (Addq, Reg "r10", Reg "rax") in
      let instr6 = Instr2 (Movq, rhs_arg, Reg "r10") in
      let instr7 = Store ("r10", 0, "rax") in
      [ instr1; instr2; instr3; instr4; instr5; instr6; instr7 ]

let rec select_tail (info : Info.t) (tail : Ir.tail) =
  match tail with
  | Return exp -> select_assign (Reg "rax") exp @ [ Jmp info.conclusion_label ]
  | Seq (stmt, tail) -> select_stmt stmt @ select_tail info tail
  | Goto label -> [ Jmp label ]
  | IfStmt (cc, atom1, atom2, thn_lbl, els_lbl) ->
      let cc = to_x86_cc cc in
      let arg1 = select_atom atom1 in
      let arg2 = select_atom atom2 in
      let instr1 = Instr2 (Cmpq, arg2, arg1) in
      let instr2 = JmpIf (cc, thn_lbl) in
      let instr3 = Jmp els_lbl in
      [ instr1; instr2; instr3 ]
  | Exit ->
      let instr1 = Instr2 (Movq, Imm 255, Reg "rdi") in
      let instr2 = Callq ("exit", 1) in
      [ instr1; instr2 ]

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
