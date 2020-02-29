module type I = sig
  type var

  type var_container

  val anonymous : var

  val is_anonymous : var Fun.predicate

  val mk : string -> var

  val is_exist_var : var Fun.predicate

  val is_free_var : var Fun.predicate

  val fresh_evar : var_container -> var

  val fresh_evars : var_container -> int -> var list

  val fresh_fvar : var_container -> var

  val fresh_fvars : var_container -> int -> var list
end

module type SubstSig = sig
  type t

  type var

  type var_container

  val empty : t

  val singleton : var -> var -> t

  val of_list : (var * var) list -> t

  val avoid : var_container -> var_container -> t

  val pp : Format.formatter -> t -> unit

  val to_string : t -> string

  val apply : t -> var -> var

  val partition : t -> t * t

  val strip : t -> t

  val mk_free_subst : var_container -> var_container -> t

  val mk_ex_subst : var_container -> var_container -> t
end

type alphabet  = string list

let roman_alphabet = [
    "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m"; "n"; "o";
    "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z";
  ]

let greek_alphabet = [
    "α"; "β"; "γ"; "δ"; "ε"; "ζ"; "η"; "θ"; "ι"; "κ"; "λ"; "μ"; "ν"; "ξ"; "ο";
    "π"; "ρ"; "ς"; "σ"; "τ"; "υ"; "φ";
    (* "χ";  *) (* Don't use χ because it just looks like x when rendered to the terminal *)
    "ψ"; "ω";
  ]

let arabic_digits = [
  "0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9";
]

module type S = sig
  module Var : sig
    include Utilsigs.BasicType

    include
      Containers.S
      with type Set.elt = t
      with type Map.key = t
      with type Hashmap.key = t
      with type Hashset.elt = t
      with type MSet.elt = t
      with type FList.t = t list

    val to_int : t -> Int.t
  end

  module Subst :
    SubstSig
    with type t = Var.t Var.Map.t
    with type var = Var.t
    with type var_container = Var.Set.t

  include I with type var = Var.t and type var_container = Var.Set.t

  val alphabet : alphabet ref
  val to_ints : Var.Set.t -> Int.Set.t
end

module H = Hashcons.Make (Strng)

type varname_class = FREE | BOUND | ANONYMOUS

let class_equal c c' =
  match (c, c') with
  | FREE, FREE | BOUND, BOUND | ANONYMOUS, ANONYMOUS -> true
  | _ -> false

let cyclic_permute ls n =
  let rec cyclic_permute ls n acc =
    if Int.equal n 0
      then List.append ls ((List.rev) acc)
      else
        match ls with
        | []    -> List.rev acc
        | l::ls -> cyclic_permute ls (n-1) (l::acc) in
  let n = n mod (List.length ls) in
  let n = if Int.( < ) n 0 then n + (List.length ls) else n in
  cyclic_permute ls n []

(* let cyclic_permute s n =
  let len = String.length s in
  let i = n mod len in
  let start = if Int.( <= ) i 0 then abs i else len - i in
  let fst = String.sub s start (len - start) in
  let snd = String.sub s 0 start in
  String.concat "" [fst; snd] *)

let mk seed anon_str classify_varname = (

  module struct

    let termtbl = H.create 997

    let _mk s = H.hashcons termtbl s

    let anonymous = _mk ""

    let mk s =
      if class_equal (classify_varname s) ANONYMOUS then anonymous else _mk s

    let alphabet = ref roman_alphabet

    module Var = struct
      module T = struct
        type t = Strng.t Hashcons.hash_consed

        let equal s s' = s == s'

        let to_string s =
          if equal s anonymous then anon_str else s.Hashcons.node

        let pp fmt s = Strng.pp fmt (to_string s)

        let compare s s' = Int.compare s.Hashcons.tag s'.Hashcons.tag

        let hash s = s.Hashcons.hkey
      end

      include T
      include Containers.Make (T)

      let to_int s = s.Hashcons.tag
    end

    type var = Var.t

    type var_container = Var.Set.t

    let is_anonymous v = Var.equal v anonymous

    let is_exist_var n =
      (not (is_anonymous n))
      && class_equal (classify_varname n.Hashcons.node) BOUND

    let is_free_var n =
      (not (is_anonymous n))
      && class_equal (classify_varname n.Hashcons.node) FREE

    let search_vars vs fresh n =
      let mk_var free lvl acc n =
        let n = match lvl with 0 -> n | _ -> n ^ "_" ^ string_of_int lvl in
        (if free then mk n else mk (n ^ "'")) :: acc
      in
      let letters = cyclic_permute !alphabet seed in
      let mk_seg free lvl =
        Blist.fold_left (mk_var free lvl) [] (List.rev letters) in
      let rec _search s acc vs n =
        match (vs, n) with
        | _, 0 -> Some (Blist.rev acc)
        | [], _ -> None
        | v :: vs, n ->
            let acc', n' =
              if Var.Set.mem v s then (acc, n) else (v :: acc, n - 1)
            in
            _search s acc' vs n'
      in
      let curr_lvl = ref 1 in
      let _fvars, _evars = (ref (mk_seg true 0), ref (mk_seg false 0)) in
      (* let () =  prerr_endline (Blist.to_string ", " Fun.id !_fvars) in *)
      let rec search s free n =
        match _search s [] (if free then !_fvars else !_evars) n with
        | Some vs -> vs
        | None ->
            _fvars := !_fvars @ mk_seg true !curr_lvl ;
            _evars := !_evars @ mk_seg false !curr_lvl ;
            incr curr_lvl ;
            search s free n
      in
      search vs fresh n

    let fresh_fvars s n = search_vars s true n

    let fresh_evars s n = search_vars s false n

    let fresh_fvar s = Blist.hd (fresh_fvars s 1)

    let fresh_evar s = Blist.hd (fresh_evars s 1)

    let to_ints vs = Var.Set.map_to Int.Set.add Int.Set.empty Var.to_int vs

    module Subst = struct
      type t = Var.t Var.Map.t

      type var = Var.t

      type var_container = Var.Set.t

      let empty = Var.Map.empty

      let singleton x y = Var.Map.add x y empty

      let of_list = Var.Map.of_list

      let pp = Var.Map.pp Var.pp

      let to_string = Var.Map.to_string Var.to_string

      let apply theta v =
        if (not (is_anonymous v)) && Var.Map.mem v theta then
          Var.Map.find v theta
        else v

      let avoid vars subvars =
        let allvars = Var.Set.union vars subvars in
        let exist_vars, free_vars =
          Pair.map Var.Set.elements (Var.Set.partition is_exist_var subvars)
        in
        let fresh_f_vars = fresh_fvars allvars (Blist.length free_vars) in
        let fresh_e_vars = fresh_evars allvars (Blist.length exist_vars) in
        Var.Map.of_list
          (Blist.append
             (Blist.combine free_vars fresh_f_vars)
             (Blist.combine exist_vars fresh_e_vars))

      let strip theta = Var.Map.filter (fun x y -> not (Var.equal x y)) theta

      let mk_subst fresh_vars avoid ts =
        let ts' = fresh_vars (Var.Set.union ts avoid) (Var.Set.cardinal ts) in
        Var.Map.of_list (Blist.combine (Var.Set.to_list ts) ts')

      let mk_free_subst = mk_subst fresh_fvars

      let mk_ex_subst = mk_subst fresh_evars

      let partition theta =
        Var.Map.partition
          (fun x y -> is_free_var x && (is_anonymous y || is_free_var y))
          theta
    end
  end
  : S )
