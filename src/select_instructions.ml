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
  | Return exp -> select_assign (Reg "rax") exp @ [ Jmp info.conclusion_label ]
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
