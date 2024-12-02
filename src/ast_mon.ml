type atom =
  | Int of int
  | Var of string

type texp = {
  exp : exp;
  ty : Type.ty;
}

and exp =
  | Int of int
  | Read
  | Add of atom * atom
  | Sub of atom * atom
  | Var of string
  | Let of string * texp * texp

type def = {
  name : string;
  params : (string * Type.ty) list;
  retty : Type.ty;
  body : texp;
}

module PP = struct
  let pp_atom formatter (atom : atom) =
    match atom with
    | Int num -> Format.fprintf formatter "%d" num
    | Var var -> Format.fprintf formatter "%s" var

  let rec pp_texp formatter { exp; ty = _ } =
    match exp with
    | Int num -> Format.fprintf formatter "%d" num
    | Read -> Format.fprintf formatter "(read)"
    | Add (a1, a2) ->
        Format.fprintf formatter "@[<2>(+@ %a@ %a)@]" pp_atom a1 pp_atom a2
    | Sub (a1, a2) ->
        Format.fprintf formatter "@[<2>(-@ %a@ %a)@]" pp_atom a1 pp_atom a2
    | Var var -> Format.fprintf formatter "%s" var
    | Let (var, init, body) ->
        Format.fprintf formatter "@[<2>(let@ ([%s@ %a])@ %a)@]" var pp_texp init
          pp_texp body

  let pp_param formatter (name, ty) =
    Format.fprintf formatter "[%s@ :@ %a]" name Type.pp ty

  let pp_params formatter params =
    match params with
    | [] -> Format.fprintf formatter ""
    | _ ->
        Format.fprintf formatter "@ ";
        Format.pp_print_list
          ~pp_sep:(fun formatter () -> Format.fprintf formatter "@ ")
          pp_param formatter params

  let pp_def formatter { name; params; retty; body } =
    Format.fprintf formatter "@[<2>(define@ (%s%a)@ :@ %a@ %a)@]" name pp_params
      params Type.pp retty pp_texp body

  let pp formatter defs =
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
      pp_def formatter defs;
    Format.fprintf formatter "@."

  let std_pp defs = pp Format.std_formatter defs
end
