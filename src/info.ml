open Utilities

type t = {
  locals_types : Type.ty MapS.t;
  prelude_label : string;
  start_label : string;
  conclusion_label : string;
  stack_space : int;
  root_stack_space : int;
  cfg : SetS.t MapS.t;
  graph : Graph.t;
  used_callee : SetS.t;
}

module PP = struct
  let pp_locals_types formatter locals_types =
    let locals_types = MapS.bindings locals_types in
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter " ")
      (fun formatter (id, ty) ->
        Format.fprintf formatter "[%s: %a]" id Type.pp ty)
      formatter locals_types

  let pp_cfg formatter cfg =
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter " ")
      (fun formatter (lbl, succs) ->
        Format.fprintf formatter "[%s: %a]" lbl pp_sets succs)
      formatter (MapS.bindings cfg)

  let pp formatter { locals_types; stack_space; root_stack_space; cfg; _ } =
    Format.fprintf formatter "@[<v>";
    Format.fprintf formatter "@[<h># locals_types: {%a}@]@," pp_locals_types
      locals_types;
    Format.fprintf formatter "@[<h># stack_space: %d@]@," stack_space;
    Format.fprintf formatter "@[<h># root_stack_space: %d@]@," root_stack_space;
    Format.fprintf formatter "@[<h># cfg: %a@]@," pp_cfg cfg;
    Format.fprintf formatter "@]"
end
