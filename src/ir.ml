type label = string

type atom =
  | Int of int
  | Var of string
  | Bool of bool
  | Void

type cc =
  | Eq
  | Lt
  | Le
  | Gt
  | Ge

type exp =
  | Int of int
  | Read
  | Add of atom * atom
  | Sub of atom * atom
  | Mul of atom * atom
  | Var of string
  | Bool of bool
  | Cmp of cc * atom * atom
  | Not of atom
  | Void
  | VectorLength of atom
  | VectorRef of atom * int
  | VectorSet of atom * int * atom
  | Allocate of int * Type.ty
  | GlobalValue of string
  | ArrayLength of atom
  | ArrayRef of atom * atom
  | ArraySet of atom * atom * atom
  | AllocateArray of atom * Type.ty

type stmt =
  | Assign of string * exp
  | ReadStmt
  | VectorSetStmt of atom * int * atom
  | Collect of atom
  | ArraySetStmt of atom * atom * atom

type tail =
  | Return of exp
  | Seq of stmt * tail
  | Goto of label
  | IfStmt of cc * atom * atom * label * label
  | Exit

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
    | Bool true -> Format.fprintf formatter "#t"
    | Bool false -> Format.fprintf formatter "#f"
    | Void -> Format.fprintf formatter "(void)"

  let pp_cc formatter cc =
    match cc with
    | Eq -> Format.fprintf formatter "eq?"
    | Lt -> Format.fprintf formatter "<"
    | Le -> Format.fprintf formatter "<="
    | Gt -> Format.fprintf formatter ">"
    | Ge -> Format.fprintf formatter ">="

  let pp_exp formatter (exp : exp) =
    match exp with
    | Int num -> Format.fprintf formatter "%d" num
    | Read -> Format.fprintf formatter "(read)"
    | Add (atom1, atom2) ->
        Format.fprintf formatter "(+ %a %a)" pp_atom atom1 pp_atom atom2
    | Sub (atom1, atom2) ->
        Format.fprintf formatter "(- %a %a)" pp_atom atom1 pp_atom atom2
    | Mul (atom1, atom2) ->
        Format.fprintf formatter "(* %a %a)" pp_atom atom1 pp_atom atom2
    | Var var -> Format.fprintf formatter "%s" var
    | Bool true -> Format.fprintf formatter "#t"
    | Bool false -> Format.fprintf formatter "#f"
    | Cmp (cc, atom1, atom2) ->
        Format.fprintf formatter "(%a %a %a)" pp_cc cc pp_atom atom1 pp_atom
          atom2
    | Not atom1 -> Format.fprintf formatter "(not %a)" pp_atom atom1
    | Void -> Format.fprintf formatter "(void)"
    | VectorLength atom1 ->
        Format.fprintf formatter "(vector-length %a)" pp_atom atom1
    | VectorRef (atom1, idx) ->
        Format.fprintf formatter "(vector-ref %a %d)" pp_atom atom1 idx
    | VectorSet (atom1, idx, atom2) ->
        Format.fprintf formatter "(vector-set! %a %d %a)" pp_atom atom1 idx
          pp_atom atom2
    | Allocate (len, ty) ->
        Format.fprintf formatter "(allocate %d %a)" len Type.pp ty
    | GlobalValue label -> Format.fprintf formatter "(global-value %s)" label
    | ArrayLength atom1 ->
        Format.fprintf formatter "(array-length %a)" pp_atom atom1
    | ArrayRef (atom1, idx) ->
        Format.fprintf formatter "(array-ref %a %a)" pp_atom atom1 pp_atom idx
    | ArraySet (atom1, idx, atom2) ->
        Format.fprintf formatter "(array-set! %a %a %a)" pp_atom atom1 pp_atom
          idx pp_atom atom2
    | AllocateArray (len, ty) ->
        Format.fprintf formatter "(allocate-array %a %a)" pp_atom len Type.pp ty

  let pp_stmt formatter (stmt : stmt) =
    match stmt with
    | Assign (var, exp) -> Format.fprintf formatter "%s = %a" var pp_exp exp
    | ReadStmt -> Format.fprintf formatter "(read)"
    | VectorSetStmt (a1, idx, a2) ->
        Format.fprintf formatter "(vector-set! %a %d %a)" pp_atom a1 idx pp_atom
          a2
    | Collect bytes -> Format.fprintf formatter "(collect %a)" pp_atom bytes
    | ArraySetStmt (a1, idx, a2) ->
        Format.fprintf formatter "(array-set! %a %a %a)" pp_atom a1 pp_atom idx
          pp_atom a2

  let rec pp_tail formatter (tail : tail) =
    match tail with
    | Return exp -> Format.fprintf formatter "return %a" pp_exp exp
    | Seq (stmt, tail) ->
        Format.fprintf formatter "%a@,%a" pp_stmt stmt pp_tail tail
    | Goto label -> Format.fprintf formatter "goto %s" label
    | IfStmt (cc, a1, a2, thn, els) ->
        Format.fprintf formatter "if (%a %a %a) goto %s; else goto %s;" pp_cc cc
          pp_atom a1 pp_atom a2 thn els
    | Exit -> Format.fprintf formatter "exit"

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

  let pp_def formatter { name; params; retty; blocks; info } =
    let pp_blocks formatter blocks =
      Format.pp_print_list
        ~pp_sep:(fun formatter () -> Format.fprintf formatter "@,")
        pp_block formatter blocks
    in
    Info.PP.pp formatter info;
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
    | ReadStmt -> SetS.empty
    | VectorSetStmt _ -> SetS.empty
    | Collect _ -> SetS.empty
    | ArraySetStmt _ -> SetS.empty

  let rec defined_vars_tail (tail : tail) =
    match tail with
    | Return _ -> SetS.empty
    | Seq (stmt, tail) ->
        SetS.union (defined_vars_stmt stmt) (defined_vars_tail tail)
    | Goto _ -> SetS.empty
    | IfStmt _ -> SetS.empty
    | Exit -> SetS.empty

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

module CFG = struct
  open Utilities

  let rec succs conclusion_label (tail : tail) =
    match tail with
    | Return _ -> SetS.singleton conclusion_label
    | Seq (_, tail) -> succs conclusion_label tail
    | Goto label -> SetS.singleton label
    | IfStmt (_, _, _, thn_lbl, els_lbl) -> SetS.of_list [ thn_lbl; els_lbl ]
    | Exit -> SetS.empty

  let make_cfg { blocks; info; _ } =
    List.fold_left
      (fun acc (lbl, tail) ->
        MapS.add lbl (succs info.conclusion_label tail) acc)
      MapS.empty blocks
end
