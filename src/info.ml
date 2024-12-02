open Utilities

type t = {
  locals_types : Type.ty MapS.t;
  prelude_label : string;
  start_label : string;
  conclusion_label : string;
  stack_space : int;
}
