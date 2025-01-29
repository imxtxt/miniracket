open Utilities
open X86

let replace_tail_jmp tail_call_conclusion blocks =
  List.map
    (fun (lbl, blk) ->
      let instrs =
        List.fold_right
          (fun instr acc ->
            match instr with
            | TailJmp (arg, _arity) ->
                tail_call_conclusion @ [ IndirectJmp arg ] @ acc
            | _ -> instr :: acc)
          blk.instrs []
      in
      (lbl, { blk with instrs }))
    blocks

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
  let init_root_stack_instrs =
    let rec helper idx =
      if idx < info.root_stack_space then
        Instr2 (Movq, Imm 0, Deref (idx, "r15")) :: helper (idx + 8)
      else []
    in
    helper 0
  in
  let prelude_instrs =
    [
      Instr1 (Pushq, Reg "rbp");
      Instr2 (Movq, Reg "rsp", Reg "rbp");
      Instr2 (Subq, Imm info.stack_space, Reg "rsp");
    ]
    @ store_used_callee
    @ (if name = "main" then
         [
           Instr2 (Movq, Imm 65536, Reg "rdi");
           Instr2 (Movq, Imm 65536, Reg "rsi");
           Callq ("initialize", 2);
           Instr2 (Movq, Global "rootstack_begin", Reg "r15");
         ]
       else [])
    @ init_root_stack_instrs
    @ [ Instr2 (Addq, Imm info.root_stack_space, Reg "r15") ]
    @ [ Jmp info.start_label ]
  in
  let prelude_block =
    (info.prelude_label, { instrs = prelude_instrs; liveafters = [] })
  in
  let conclusion_instrs =
    [ Instr2 (Subq, Imm info.root_stack_space, Reg "r15") ]
    @ load_used_callee
    @ [
        Instr2 (Addq, Imm info.stack_space, Reg "rsp");
        Instr1 (Popq, Reg "rbp");
        Retq;
      ]
  in
  let conclusion_block =
    (info.conclusion_label, { instrs = conclusion_instrs; liveafters = [] })
  in
  let blocks =
    replace_tail_jmp (Base.List.drop_last_exn conclusion_instrs) blocks
  in
  { name; blocks = [ prelude_block ] @ blocks @ [ conclusion_block ]; info }

let run defs = List.map prelude_conclusion_def defs
