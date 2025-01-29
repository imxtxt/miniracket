open X86

let patch_instr instr =
  match instr with
  | Instr2 (Cmpq, arg, Imm imm) ->
      let instr1 = Instr2 (Movq, Imm imm, Reg RegUse.patch_reg) in
      let instr2 = Instr2 (Cmpq, arg, Reg RegUse.patch_reg) in
      [ instr1; instr2 ]
  | Instr2 (Movzbq, arg, Deref (i, r)) ->
      let instr1 = Instr2 (Movzbq, arg, Reg RegUse.patch_reg) in
      let instr2 = Instr2 (Movq, Reg RegUse.patch_reg, Deref (i, r)) in
      [ instr1; instr2 ]
  | Instr2 (Movq, Reg r1, Reg r2) when r1 = r2 -> []
  | Instr2 (Movq, Deref (i1, r1), Deref (i2, r2)) when i1 = i2 && r1 = r2 -> []
  | Instr2 (op2, Deref (i1, r1), Deref (i2, r2)) ->
      let instr1 = Instr2 (Movq, Deref (i1, r1), Reg RegUse.patch_reg) in
      let instr2 = Instr2 (op2, Reg RegUse.patch_reg, Deref (i2, r2)) in
      [ instr1; instr2 ]
  | Instr2 (op2, Imm imm, Deref (i2, r2)) when imm > 65536 ->
      let instr1 = Instr2 (Movq, Imm imm, Reg RegUse.patch_reg) in
      let instr2 = Instr2 (op2, Reg RegUse.patch_reg, Deref (i2, r2)) in
      [ instr1; instr2 ]
  | Instr2 (Leaq, arg, Deref (offset, reg)) ->
      let instr1 = Instr2 (Leaq, arg, Reg RegUse.patch_reg) in
      let instr2 = Instr2 (Movq, Reg RegUse.patch_reg, Deref (offset, reg)) in
      [ instr1; instr2 ]
  | Instr2 (op2, arg1, arg2) -> [ Instr2 (op2, arg1, arg2) ]
  | Instr1 (op1, arg1) -> [ Instr1 (op1, arg1) ]
  | Callq (lbl, arity) -> [ Callq (lbl, arity) ]
  | Retq -> [ Retq ]
  | Jmp lbl -> [ Jmp lbl ]
  | JmpIf (cc, lbl) -> [ JmpIf (cc, lbl) ]
  | Set (cc, arg) -> [ Set (cc, arg) ]
  | Load (offset, base, dest) -> [ Load (offset, base, dest) ]
  | Store (src, offset, base) -> [ Store (src, offset, base) ]
  | IndirectCallq (arg, arity) -> [ IndirectCallq (arg, arity) ]
  | TailJmp (arg, arity) ->
      let instr1 = Instr2 (Movq, arg, Reg RegUse.patch_reg) in
      let instr2 = TailJmp (Reg RegUse.patch_reg, arity) in
      [ instr1; instr2 ]
  | IndirectJmp arg -> [ IndirectJmp arg ]

let patch_block { instrs; liveafters } =
  let instrs = List.map patch_instr instrs |> List.flatten in
  { instrs; liveafters }

let patch_def { name; blocks; info } =
  let blocks = List.map (fun (lbl, blk) -> (lbl, patch_block blk)) blocks in
  { name; blocks; info }

let run defs = List.map patch_def defs
