let parse filename =
  let in_handle = open_in filename in
  let lexbuf = Lexing.from_channel in_handle in
  let ast = Parser.start Lexer.token lexbuf in
  close_in in_handle;
  ast

let _ =
  parse Sys.argv.(1) 
  |> Type_check.run 
  |> Uniquify.run
  |> Remove_complex_operands.run
  |> Explicate_control.run
  |> Inject_info.run
  |> Select_instructions.run
  |> Uncover_live.run
  |> Interference_graph.run
  |> Allocate_registers.run
  |> Patch_instructions.run
  |> Prelude_conclusion.run
  |> X86.PP.std_pp