open Utilities
open X86

let interference_graph_block locals_types (graph : Graph.t) (block : block) =
  List.iter2
    (fun instr liveafter ->
      match instr with
      | Instr2 (Movq, Var s, Var d)
      | Instr2 (Movq, Var s, Reg d)
      | Instr2 (Movq, Reg s, Reg d)
      | Instr2 (Movq, Reg s, Var d) ->
          SetS.iter
            (fun v -> if v <> s && v <> d then Graph.add_edge graph d v)
            liveafter
      | _ -> (
          let instr_write = Uncover_live.instr_write instr in
          SetS.iter
            (fun d ->
              SetS.iter
                (fun v -> if v <> d then Graph.add_edge graph d v)
                liveafter)
            instr_write;
          match instr with
          | Callq ("collect", 2) ->
              List.iter
                (fun d ->
                  SetS.iter
                    (fun v ->
                      if
                        v <> d && MapS.mem v locals_types
                        && Type.is_pointer (MapS.find v locals_types)
                      then Graph.add_edge graph d v)
                    liveafter)
                RegUse.callee_saved_regs
          | _ -> ()))
    block.instrs block.liveafters

let interference_graph_def { name; blocks; info } =
  let graph = info.graph in
  List.iter (fun reg -> Graph.add_node graph reg) RegUse.all_regs;
  MapS.iter (fun var _ -> Graph.add_node graph var) info.locals_types;
  List.iter
    (fun (_, blk) -> interference_graph_block info.locals_types graph blk)
    blocks;
  let info = { info with graph } in
  { name; blocks; info }

let run defs = List.map interference_graph_def defs
