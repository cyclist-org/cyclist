open Lib

module Make (T : Utilsigs.BasicType) = struct
  include Map.Make (T)

  let equal eq m m' = m == m' || equal eq m m'

  let compare comp m m' = if m == m' then 0 else compare comp m m'

  let hash h m =
    fold (fun k v a -> genhash a (genhash (T.hash k) (h v))) m 0x9e3779b9

  let rec fixpoint eq f x =
    let y = f x in
    if equal eq x y then x else fixpoint eq f y

  (* NB this prioritises bindings of the first argument *)
  let union m m' = fold add m m'

  let of_list l = Blist.fold_left (fun m (k, v) -> add k v m) empty l

  let to_list = bindings

  exception Found

  let find_map f (m : 'a t) =
    let found = ref None in
    try
      iter
        (fun k v ->
          if f k v then (
            found := Some (k, v) ;
            raise Found ) )
        m ;
      None
    with Found -> !found

  let pp pp_val fmt m =
    let aux fmt m =
      iter (fun k v -> Format.fprintf fmt "@[(%a->%a);@]@ " T.pp k pp_val v) m
    in
    Format.fprintf fmt "@[%a@]" aux m

  let to_string val_to_string m =
    let pp_val fmt v = Format.pp_print_string fmt (val_to_string v) in
    mk_to_string (pp pp_val) m

  let submap eq m m' = for_all (fun k v -> mem k m' && eq v (find k m')) m

  let add_bindings bs m = List.fold_left (fun m (k, v) -> add k v m) m bs
end
