type texp = {
  exp : exp;
  ty : Type.ty;
}

and cc =
  | Eq
  | Lt
  | Le
  | Gt
  | Ge

and exp =
  | Int of int
  | Read
  | Add of texp * texp
  | Sub of texp * texp
  | Var of string
  | GetBang of string
  | Let of string * texp * texp
  | Bool of bool
  | If of texp * texp * texp
  | Cmp of cc * texp * texp
  | Not of texp
  | SetBang of string * texp
  | Begin of texp list * texp
  | WhileLoop of texp * texp
  | Void

type def = {
  name : string;
  params : (string * Type.ty) list;
  retty : Type.ty;
  body : texp;
}

module PP = struct
  let pp_cc formatter cc =
    match cc with
    | Eq -> Format.fprintf formatter "eq?"
    | Lt -> Format.fprintf formatter "<"
    | Le -> Format.fprintf formatter "<="
    | Gt -> Format.fprintf formatter ">"
    | Ge -> Format.fprintf formatter ">="

  let rec pp_texp formatter { exp; ty = _ } =
    match exp with
    | Int num -> Format.fprintf formatter "%d" num
    | Read -> Format.fprintf formatter "(read)"
    | Add (e1, e2) ->
        Format.fprintf formatter "@[<2>(+@ %a@ %a)@]" pp_texp e1 pp_texp e2
    | Sub (e1, e2) ->
        Format.fprintf formatter "@[<2>(-@ %a@ %a)@]" pp_texp e1 pp_texp e2
    | Var var -> Format.fprintf formatter "%s" var
    | GetBang var -> Format.fprintf formatter "@[(get!@ %s)@]" var
    | Let (var, init, body) ->
        Format.fprintf formatter "@[<2>(let@ ([%s@ %a])@ %a)@]" var pp_texp init
          pp_texp body
    | Bool true -> Format.fprintf formatter "#t"
    | Bool false -> Format.fprintf formatter "#f"
    | If (cnd, thn, els) ->
        Format.fprintf formatter "@[<2>(if@ %a@ %a@ %a)@]" pp_texp cnd pp_texp
          thn pp_texp els
    | Cmp (cc, e1, e2) ->
        Format.fprintf formatter "@[<2>(%a@ %a@ %a)@]" pp_cc cc pp_texp e1
          pp_texp e2
    | Not e -> Format.fprintf formatter "@[<2>(not@ %a)@]" pp_texp e
    | SetBang (var, rhs) ->
        Format.fprintf formatter "@[<2>(set!@ %s@ %a)@]" var pp_texp rhs
    | Begin (es, e) ->
        Format.fprintf formatter "@[<v 2>(begin@ %a@ %a)@]" pp_texps es pp_texp
          e
    | WhileLoop (cnd, body) ->
        Format.fprintf formatter "@[<v 2>(while@ %a@ %a)@]" pp_texp cnd pp_texp
          body
    | Void -> Format.fprintf formatter "@[(void)@]"

  and pp_texps formatter exps =
    Format.pp_print_list
      ~pp_sep:(fun formatter () -> Format.fprintf formatter "@ ")
      pp_texp formatter exps

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
