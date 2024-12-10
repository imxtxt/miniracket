type label = string

type arg =
  | Imm of int
  | Reg of string
  | Var of string
  | Deref of int * string

type op2 =
  | Addq
  | Subq
  | Movq

type op1 =
  | Pushq
  | Popq

type instr =
  | Instr2 of op2 * arg * arg
  | Instr1 of op1 * arg
  | Callq of label * int
  | Retq
  | Jmp of label

type block = {
  instrs : instr list;
  liveafters : Utilities.SetS.t list;
}

type def = {
  name : label;
  blocks : (label * block) list;
  info : Info.t;
}

module PP = struct
  open Utilities

  let pp_arg formatter arg =
    match arg with
    | Imm imm -> Format.fprintf formatter "$%d" imm
    | Reg reg -> Format.fprintf formatter "%%%s" reg
    | Var var -> Format.fprintf formatter "%s" var
    | Deref (idx, reg) -> Format.fprintf formatter "%d(%%%s)" idx reg

  let pp_opcode2 formatter op2 =
    match op2 with
    | Addq -> Format.fprintf formatter "addq"
    | Subq -> Format.fprintf formatter "subq"
    | Movq -> Format.fprintf formatter "movq"

  let pp_opcode1 formatter op1 =
    match op1 with
    | Pushq -> Format.fprintf formatter "pushq"
    | Popq -> Format.fprintf formatter "popq"

  let pp_instr formatter instr =
    match instr with
    | Instr2 (op2, arg1, arg2) ->
        Format.fprintf formatter "%a %a, %a" pp_opcode2 op2 pp_arg arg1 pp_arg
          arg2
    | Instr1 (op1, arg1) ->
        Format.fprintf formatter "%a %a" pp_opcode1 op1 pp_arg arg1
    | Callq (label, _) ->
        Format.fprintf formatter "callq %s" (mangle_label label)
    | Retq -> Format.fprintf formatter "retq"
    | Jmp label -> Format.fprintf formatter "jmp %s" (mangle_label label)

  let pp_instrs formatter { instrs; liveafters } =
    match liveafters with
    | [] ->
        Format.pp_print_list
          ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
          pp_instr formatter instrs
    | _ ->
        let live_instrs = List.combine instrs liveafters in
        Format.pp_print_list
          ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
          (fun formatter (instr, liveafter) ->
            pp_instr formatter instr;
            Format.fprintf formatter "@,#";
            pp_sets formatter liveafter)
          formatter live_instrs

  let pp_block formatter (label, blk) =
    Format.fprintf formatter "@[<v 2>%s:@,%a@]" (mangle_label label) pp_instrs
      blk

  let pp_def formatter { name; blocks; info } =
    let pp_blocks formatter blocks =
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
        pp_block formatter blocks
    in
    Info.PP.pp formatter info;
    if name = "main" then
      Format.fprintf formatter "@[<v>.global %s@,%s:@,%a@]" (mangle_label name)
        (mangle_label name) pp_blocks blocks
    else
      Format.fprintf formatter "@[<v>%s:@,%a@]" (mangle_label name) pp_blocks
        blocks

  let pp formatter defs =
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
      pp_def formatter defs;
    Format.fprintf formatter "@."

  let std_pp defs = pp Format.std_formatter defs
end

module RegUse = struct
  let patch_reg = "r11"

  (* callee: rsp, rbp, r15
     caller: r11 *)
  let unused_regs = [ "rsp"; "rbp"; "r15"; "r11" ]
  let arg_regs = [ "rdi"; "rsi"; "rdx"; "rcx"; "r8"; "r9" ]
  let caller_saved_regs = arg_regs @ [ "r10"; "rax" ]
  let callee_saved_regs = [ "rbx"; "r12"; "r13"; "r14" ]
  let allocatable_regs = caller_saved_regs @ callee_saved_regs
  let allocatable_regs_len = List.length allocatable_regs
  let all_regs = allocatable_regs @ unused_regs

  let reg2num =
    let allocatable_regs_assoc =
      List.mapi (fun i r -> (r, i)) allocatable_regs
    in
    let unused_regs_assoc =
      List.(mapi (fun i r -> (r, i - length unused_regs)) unused_regs)
    in
    let assoc = allocatable_regs_assoc @ unused_regs_assoc in
    fun reg -> List.assoc reg assoc

  let num2reg =
    let allocatable_regs_assoc =
      List.mapi (fun i r -> (i, r)) allocatable_regs
    in
    let unused_regs_assoc =
      List.(mapi (fun i r -> (i - length unused_regs, r)) unused_regs)
    in
    let assoc = allocatable_regs_assoc @ unused_regs_assoc in
    fun num -> List.assoc num assoc
end
