type ty =
  | Integer
  | Boolean
  | Void
  | Function of ty list * ty

let rec pp formatter ty =
  match ty with
  | Integer -> Format.fprintf formatter "Integer"
  | Boolean -> Format.fprintf formatter "Boolean"
  | Void -> Format.fprintf formatter "Void"
  | Function (ts, t) ->
      let pp_ts formatter ts =
        Format.pp_print_list
          ~pp_sep:(fun formatter () -> Format.fprintf formatter "@ ")
          pp formatter ts
      in
      Format.fprintf formatter "(%a@ ->@ %a)" pp_ts ts pp t

let ( = ) t1 t2 =
  match (t1, t2) with
  | Integer, Integer -> true
  | Boolean, Boolean -> true
  | Void, Void -> true
  | _ -> false
