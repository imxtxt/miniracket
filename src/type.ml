type ty =
  | Integer
  | Boolean
  | Void
  | Vector of ty list
  | Array of ty
  | Function of ty list * ty

let rec pp formatter ty =
  match ty with
  | Integer -> Format.fprintf formatter "Integer"
  | Boolean -> Format.fprintf formatter "Boolean"
  | Void -> Format.fprintf formatter "Void"
  | Vector ts -> Format.fprintf formatter "(Vector %a)" pp_ts ts
  | Array ty -> Format.fprintf formatter "(Array %a)" pp ty
  | Function (ts, t) -> Format.fprintf formatter "(%a -> %a)" pp_ts ts pp t

and pp_ts formatter ts =
  Format.pp_print_list
    ~pp_sep:(fun formatter () -> Format.fprintf formatter " ")
    pp formatter ts

let rec ( = ) t1 t2 =
  match (t1, t2) with
  | Integer, Integer -> true
  | Boolean, Boolean -> true
  | Void, Void -> true
  | Vector tys1, Vector tys2 -> List.for_all2 ( = ) tys1 tys2
  | Array t1, Array t2 -> t1 = t2
  | Function (ps1, rt1), Function (ps2, rt2) ->
      List.for_all2 ( = ) ps1 ps2 && rt1 = rt2
  | _ -> false

let is_pointer (ty : ty) =
  match ty with
  | Integer -> false
  | Boolean -> false
  | Void -> false
  | Vector _ -> true
  | Array _ -> true
  | Function _ -> false
