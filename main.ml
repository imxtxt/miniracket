let parse filename =
  let in_handle = open_in filename in
  let lexbuf = Lexing.from_channel in_handle in
  let ast = Parser.start Lexer.token lexbuf in
  close_in in_handle;
  ast

let _ =
  let open Pass in
  parse Sys.argv.(1)
  |> TypeCheck.run 
  |> Uniquify.run 
  |> RemoveComplexOperands.run
  |> ExplicateControl.run
  |> LocalsTypes.run
  |> SelectInstructions.run 
  |> AssignHomes.run
  |> PatchInstructions.run
  |> PreludeConclusion.run
  |> X86.PP.std_pp
