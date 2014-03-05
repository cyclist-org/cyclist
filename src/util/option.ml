let is_none = function None -> true | Some _ -> false
let is_some = function Some _ -> true | None -> false

let pred p x =
  if p x then Some x else None

let get = function
  | Some(x) -> x
  | None -> invalid_arg "Option.get"

let map f = function
  | None -> None
  | Some x -> f x

let list_get l = Blist.rev_map get (Blist.rev_filter is_some l)

let some x = Some x

let dest none some = function
  | None -> none
  | Some x -> some x

