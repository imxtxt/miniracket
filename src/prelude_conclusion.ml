open Utilities
open X86

let prelude_conclusion_def { name; blocks; info } =
  let used_callee = SetS.elements info.used_callee in
  let store_used_callee, load_used_callee =
    List.mapi
      (fun idx reg ->
        let offset = -8 * (idx + 1) in
        let store = Instr2 (Movq, Reg reg, Deref (offset, "rbp")) in
        let load = Instr2 (Movq, Deref (offset, "rbp"), Reg reg) in
        (store, load))
      used_callee
    |> List.split
  in
  let prelude_instrs =
    [
      Instr1 (Pushq, Reg "rbp");
      Instr2 (Movq, Reg "rsp", Reg "rbp");
      Instr2 (Subq, Imm info.stack_space, Reg "rsp");
    ]
    @ store_used_callee @ [ Jmp info.start_label ]
  in
  let prelude_block =
    (info.prelude_label, { instrs = prelude_instrs; liveafters = [] })
  in
  let conclusion_instrs =
    load_used_callee
    @ [
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
