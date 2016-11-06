(** Functions for manipulating optional values. *)

let is_none = function None -> true | Some _ -> false
let is_some = function Some _ -> true | None -> false

(** [pred p x] returns [Some x] if [p x] else [None]. *)
let pred p x =
  if p x then Some x else None

let mk b x =
  if b then Some x else None
	
let mk_lazily b f =
	if b then Some (f()) else None

let get = function
  | Some(x) -> x
  | None -> invalid_arg "Option.get"

let map f = function
  | None -> None
  | Some x -> Some (f x)

let list_get l = Blist.rev_map get (Blist.rev_filter is_some l)

let some x = Some x

let dest none some = function
  | None -> none
  | Some x -> some x

let dest_lazily none some = function
  | None -> none ()
  | Some x -> some x

let pred_dest p = function
  | None -> false
  | Some x -> p x

let flatten = function
  | Some (Some x) -> Some x
  | _ -> None

let bind f o = flatten (map f o)

let pp pp_elem fmt = function
  | None -> Format.fprintf fmt "@[None@]"
  | Some x -> Format.fprintf fmt "@[Some (%a)@]" pp_elem x

let iter f = function
  | None -> ()
  | Some x -> f x

let fold f o a = 
  match o with
  | None -> a
  | Some x -> f x a
