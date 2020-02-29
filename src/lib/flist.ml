open Misc

module Make (T : Utilsigs.BasicType) :
  Utilsigs.BasicType with type t = T.t list = struct
  type t = T.t list

  let rec compare l l' =
    if l == l' then 0
    else
      match (l, l') with
      | [], [] -> 0
      | [], _ -> -1
      | _, [] -> 1
      | hd :: tl, hd' :: tl' -> (
        match T.compare hd hd' with 0 -> compare tl tl' | n -> n )

  let rec equal l l' =
    l == l'
    ||
    match (l, l') with
    | [], [] -> true
    | hd :: tl, hd' :: tl' -> T.equal hd hd' && equal tl tl'
    | [], _ | _, [] -> false

  let hash l = Blist.fold_left (fun h v -> genhash h (T.hash v)) 0x9e3779b9 l

  (* let hash l =                                    *)
  (*   let rec aux h = function                      *)
  (*     | [] -> h                                   *)
  (*     | x::xs -> aux (genhash h (T.hash x)) xs in *)
  (*   aux 0x9e3779b9 l                              *)
  (* let hash = Hashtbl.hash *)

  let pp fmt l = Format.fprintf fmt "@[[%a]@]" (Blist.pp pp_semicolonsp T.pp) l

  let to_string = mk_to_string pp
end
