open Utilities
open Ir

let update_locals_types ({ Ir.info; _ } as def) =
  let defined_vars = DefinedVar.defined_vars_def def in
  let locals_types =
    MapS.filter (fun var _ -> SetS.mem var defined_vars) info.locals_types
  in
  let info = { info with locals_types } in
  { def with info }

let update_cfg ({ Ir.info; _ } as def) =
  let cfg = CFG.make_cfg def in
  let info = { info with cfg } in
  { def with info }

let updata_info def = update_locals_types def |> update_cfg
let run defs = List.map updata_info defs
