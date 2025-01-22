open Utilities

exception LivenessError

open X86

let arg_loc (arg : arg) =
  match arg with
  | Imm _ -> SetS.empty
  | Reg reg -> SetS.singleton reg
  | Var var -> SetS.singleton var
  | Deref _ -> raise LivenessError
  | ByteReg breg ->
      let reg = RegUse.bytereg2reg breg in
      SetS.singleton reg
  | Global _ -> SetS.empty

let instr_read (instr : instr) =
  match instr with
  | Instr2
      ( (Addq | Subq | Xorq | Cmpq | Sarq | Andq | Orq | Imulq | Salq | Shrq),
        arg1,
        arg2 ) ->
      SetS.union (arg_loc arg1) (arg_loc arg2)
  | Instr2 ((Movq | Movzbq), arg1, _) -> arg_loc arg1
  | Instr1 ((Pushq | Popq), _) -> raise LivenessError
  | Callq (_, arity) ->
      let open Base in
      SetS.of_list (List.take RegUse.arg_regs arity)
  | Retq -> raise LivenessError
  | Jmp _ -> SetS.empty
  | JmpIf _ -> SetS.empty
  | Set _ -> SetS.empty
  | Load (_offset, base, _dest) -> SetS.singleton base
  | Store (src, _offset, base) -> SetS.of_list [ src; base ]

let instr_write (instr : instr) =
  match instr with
  | Instr2
      ( ( Addq | Subq | Movq | Xorq | Movzbq | Sarq | Andq | Orq | Imulq | Salq
        | Shrq ),
        _,
        arg2 ) ->
      arg_loc arg2
  | Instr2 (Cmpq, _, _) -> SetS.empty
  | Instr1 ((Pushq | Popq), _) -> raise LivenessError
  | Callq _ -> SetS.of_list RegUse.caller_saved_regs
  | Retq -> raise LivenessError
  | Jmp _ -> SetS.empty
  | JmpIf _ -> SetS.empty
  | Set (_, arg) -> arg_loc arg
  | Load (_offset, _base, dest) -> SetS.singleton dest
  | Store (_src, _offset, _base) -> SetS.empty

let liveness_instrs (liveafter : SetS.t) (instrs : X86.instr list) =
  List.fold_right
    (fun instr (liveafter, liveafters) ->
      let liveafters = liveafter :: liveafters in
      let livebefore =
        SetS.diff liveafter (instr_write instr) |> SetS.union (instr_read instr)
      in
      (livebefore, liveafters))
    instrs (liveafter, [])

let rec liveness_blocks cfg liveinfo blocks =
  let new_liveinfo, new_blocks =
    List.fold_right
      (fun (lbl, blk) (new_liveinfo, new_blocks) ->
        let succs = MapS.find lbl cfg in
        let liveafter =
          SetS.fold
            (fun succ acc -> SetS.union acc (MapS.find succ liveinfo))
            succs SetS.empty
        in
        let livebefore, liveafters = liveness_instrs liveafter blk.instrs in
        let new_liveinfo = MapS.add lbl livebefore new_liveinfo in
        let new_blocks = (lbl, { blk with liveafters }) :: new_blocks in
        (new_liveinfo, new_blocks))
      blocks (liveinfo, [])
  in
  if MapS.equal SetS.equal liveinfo new_liveinfo then new_blocks
  else liveness_blocks cfg new_liveinfo blocks

let liveness_def { X86.name; blocks; info } =
  let liveinfo =
    MapS.add info.conclusion_label (SetS.of_list [ "rsp"; "rax" ]) MapS.empty
  in
  let liveinfo =
    List.fold_left
      (fun liveinfo (lbl, _) -> MapS.add lbl SetS.empty liveinfo)
      liveinfo blocks
  in
  let blocks = liveness_blocks info.cfg liveinfo blocks in
  { name; blocks; info }

let run defs = List.map liveness_def defs
