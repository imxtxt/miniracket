type label = string

type atom =
  | Int of int
  | Var of string

type exp =
  | Int of int
  | Read
  | Add of atom * atom
  | Sub of atom * atom
  | Var of string

type stmt = Assign of string * exp

type tail =
  | Return of exp
  | Seq of stmt * tail

type def = {
  name : label;
  params : (string * Type.ty) list;
  retty : Type.ty;
  blocks : (label * tail) list;
  info : Info.t;
}

module PP = struct
  let pp_atom formatter (atom : atom) =
    match atom with
    | Int num -> Format.fprintf formatter "%d" num
    | Var var -> Format.fprintf formatter "%s" var

  let pp_exp formatter (exp : exp) =
    match exp with
    | Int num -> Format.fprintf formatter "%d" num
    | Read -> Format.fprintf formatter "(read)"
    | Add (atom1, atom2) ->
        Format.fprintf formatter "(+ %a %a)" pp_atom atom1 pp_atom atom2
    | Sub (atom1, atom2) ->
        Format.fprintf formatter "(- %a %a)" pp_atom atom1 pp_atom atom2
    | Var var -> Format.fprintf formatter "%s" var

  let pp_stmt formatter (stmt : stmt) =
    match stmt with
    | Assign (var, exp) -> Format.fprintf formatter "%s = %a" var pp_exp exp

  let rec pp_tail formatter (tail : tail) =
    match tail with
    | Return exp -> Format.fprintf formatter "return %a" pp_exp exp
    | Seq (stmt, tail) ->
        Format.fprintf formatter "%a@,%a" pp_stmt stmt pp_tail tail

  let pp_block formatter (label, tail) =
    Format.fprintf formatter "@[<v 2>%s:@,%a@]" label pp_tail tail

  let pp_param formatter (name, ty) =
    Format.fprintf formatter "[%s : %a]" name Type.pp ty

  let pp_params formatter params =
    match params with
    | [] -> Format.fprintf formatter ""
    | _ ->
        Format.fprintf formatter " ";
        Format.pp_print_list
          ~pp_sep:(fun formatter () -> Format.fprintf formatter " ")
          pp_param formatter params

  let pp_def formatter { name; params; retty; blocks; info = _ } =
    let pp_blocks formatter blocks =
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
        pp_block formatter blocks
    in
    Format.fprintf formatter "@[<v>define (%s%a) : %a {@,%a@,}@]" name pp_params
      params Type.pp retty pp_blocks blocks

  let pp formatter defs =
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
      pp_def formatter defs;
    Format.fprintf formatter "@."

  let std_pp defs = pp Format.std_formatter defs
end

module DefinedVar = struct
  open Utilities

  let defined_vars_stmt (stmt : stmt) =
    match stmt with
    | Assign (var, _) -> SetS.singleton var

  let rec defined_vars_tail (tail : tail) =
    match tail with
    | Return _ -> SetS.empty
    | Seq (stmt, tail) ->
        SetS.union (defined_vars_stmt stmt) (defined_vars_tail tail)

  let defined_vars_def { params; blocks; _ } =
    let defined_vars =
      List.fold_left
        (fun acc (_, tail) -> SetS.union acc (defined_vars_tail tail))
        SetS.empty blocks
    in
    List.fold_left
      (fun acc (param, _) -> SetS.add param acc)
      defined_vars params
end
