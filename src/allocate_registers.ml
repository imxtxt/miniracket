open Utilities
open X86

module AssignHomes = struct
  let assign_home_arg env arg =
    match arg with
    | Imm imm -> Imm imm
    | Reg reg -> Reg reg
    | Var var -> MapS.find var env
    | Deref (idx, reg) -> Deref (idx, reg)
    | ByteReg reg -> ByteReg reg
    | Global label -> Global label

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
    | JmpIf (cc, lbl) -> JmpIf (cc, lbl)
    | Set (cc, arg1) ->
        let arg1 = assign_home_arg env arg1 in
        Set (cc, arg1)
    | Load (offset, base, dest) -> Load (offset, base, dest)
    | Store (src, offset, base) -> Store (src, offset, base)
    | IndirectCallq (arg, arity) ->
        let arg = assign_home_arg env arg in
        IndirectCallq (arg, arity)
    | TailJmp (arg, arity) ->
        let arg = assign_home_arg env arg in
        TailJmp (arg, arity)
    | IndirectJmp arg ->
        let arg = assign_home_arg env arg in
        IndirectJmp arg

  let assign_home_block env { instrs; liveafters = _ } =
    let instrs = List.map (assign_home_instr env) instrs in
    { instrs; liveafters = [] }
end

let color_graph (igraph : Graph.t) (locals_types : Type.ty MapS.t) =
  let locals = MapS.bindings locals_types |> List.map fst in
  (* initialize color *)
  let color = Hashtbl.create 10 in
  List.iter
    (fun reg -> Hashtbl.add color reg (RegUse.reg2num reg))
    RegUse.all_regs;
  (* initialize saturation *)
  let saturation = Hashtbl.create 10 in
  List.iter (fun reg -> Hashtbl.add saturation reg SetI.empty) RegUse.all_regs;
  List.iter (fun var -> Hashtbl.add saturation var SetI.empty) locals;
  let update_saturation node =
    List.iter
      (fun adj ->
        let adj_saturation = Hashtbl.find saturation adj in
        let new_adj_saturation =
          SetI.(union adj_saturation (singleton (Hashtbl.find color node)))
        in
        Hashtbl.add saturation adj new_adj_saturation)
      (Graph.adjs igraph node)
  in
  List.iter update_saturation RegUse.all_regs;
  let rec assign_color (nodes : SetS.t) =
    let pop nodes =
      SetS.fold
        (fun node acc ->
          match acc with
          | None -> Some node
          | Some old_node ->
              if
                SetI.cardinal (Hashtbl.find saturation node)
                > SetI.cardinal (Hashtbl.find saturation old_node)
              then Some node
              else Some old_node)
        nodes None
    in
    match pop nodes with
    | None -> ()
    | Some node ->
        let node_saturation = Hashtbl.find saturation node in
        let rec choose_num idx =
          if SetI.mem idx node_saturation then choose_num (idx + 1) else idx
        in
        let num = choose_num 0 in
        Hashtbl.add color node num;
        update_saturation node;
        assign_color (SetS.remove node nodes)
  in
  assign_color (SetS.of_list locals);
  (* remove registers from color *)
  List.iter (fun reg -> Hashtbl.remove color reg) RegUse.all_regs;
  Hashtbl.to_seq color |> List.of_seq

let allocate_registers_def { X86.name; blocks; info } =
  let color = color_graph info.graph info.locals_types in
  let gather_used_callee color =
    let callee_nums =
      List.map RegUse.reg2num RegUse.callee_saved_regs |> SetI.of_list
    in
    List.fold_left
      (fun acc (_, num) ->
        if SetI.mem num callee_nums then SetS.add (RegUse.num2reg num) acc
        else acc)
      SetS.empty color
  in
  let gather_spilled_nums color =
    List.fold_left
      (fun acc (var, num) ->
        let var_type = MapS.find var info.locals_types in
        if num >= RegUse.allocatable_regs_len && not (Type.is_pointer var_type)
        then SetI.add num acc
        else acc)
      SetI.empty color
  in
  let gather_root_spilled_nums color =
    List.fold_left
      (fun acc (var, num) ->
        let var_type = MapS.find var info.locals_types in
        if num >= RegUse.allocatable_regs_len && Type.is_pointer var_type then
          SetI.add num acc
        else acc)
      SetI.empty color
  in
  let used_callee = gather_used_callee color in
  let spilled_nums = gather_spilled_nums color in
  let spilled_map =
    SetI.elements spilled_nums |> List.mapi (fun idx num -> (num, idx))
  in
  let root_spilled_nums = gather_root_spilled_nums color in
  let root_spilled_map =
    SetI.elements root_spilled_nums |> List.mapi (fun idx num -> (num, idx))
  in
  let stack_space =
    let stack_size = SetS.cardinal used_callee + SetI.cardinal spilled_nums in
    align_stack (stack_size * 8)
  in
  let root_stack_space = SetI.cardinal root_spilled_nums * 8 in
  let new_color =
    List.fold_left
      (fun acc (var, num) ->
        let var_type = MapS.find var info.locals_types in
        if num < RegUse.allocatable_regs_len then
          MapS.add var (Reg (RegUse.num2reg num)) acc
        else if Type.is_pointer var_type then
          let idx = List.assoc num root_spilled_map in
          MapS.add var (Deref ((idx + 1) * -8, "r15")) acc
        else
          let idx = List.assoc num spilled_map in
          let offset = (-8 * SetS.cardinal used_callee) - (8 * (1 + idx)) in
          MapS.add var (Deref (offset, "rbp")) acc)
      MapS.empty color
  in
  let blocks =
    List.map
      (fun (lbl, blk) -> (lbl, AssignHomes.assign_home_block new_color blk))
      blocks
  in
  let info = { info with stack_space; root_stack_space; used_callee } in
  { X86.name; blocks; info }

let run defs = List.map allocate_registers_def defs
