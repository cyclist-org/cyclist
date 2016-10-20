module type Kernel = 
  sig
    val map : string Int.Hashmap.t
    val inv_map : int Strng.Hashmap.t
    val max_var : int ref
    val min_var : int ref
    val default_varname : bool -> string
    val unnamed_varname : string
  end

module type I =
  sig
    type var
    type var_container

    val unnamed : var
    val is_unnamed : var Fun.predicate
    val present : var Fun.predicate
    val name_present : string Fun.predicate
    val get_var : string -> var
    val mk_var : string -> bool -> var
    val is_exist_var : var Fun.predicate
    val is_free_var : var Fun.predicate
    val is_exist_name : string Fun.predicate
    val is_free_name : string Fun.predicate
    val fresh_evar : var_container -> var
    val fresh_evars : var_container -> int -> var list
    val fresh_fvar : var_container -> var
    val fresh_fvars : var_container -> int -> var list
  end

module type SubstSig =
  sig
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

module type S =
  sig
    module Var : 
      sig
        include Utilsigs.BasicType
        
        include Containers.S 
          with type Set.elt=t
          with type Map.key=t
          with type Hashmap.key=t
          with type Hashset.elt=t
          with type MSet.elt=t
          with type FList.t=t list

        val to_int : t -> Int.t
      end

    module Subst : SubstSig 
      with type t = Var.t Var.Map.t
      with type var = Var.t
      with type var_container = Var.Set.t

    include I with type var = Var.t and type var_container = Var.Set.t
    
    val to_ints : Var.Set.t -> Int.Set.t
  end

module Make(K : Kernel) =
  struct
    let unnamed = 0
    let is_unnamed v = (v = unnamed)
      
    module Var =
      struct
        module M = 
          struct
            type t = int
            let compare v v' = Int.compare v v'
            let equal v v' = Int.equal v v'
            let hash v = Int.hash v
            let to_string v = 
              if is_unnamed v then K.unnamed_varname else Int.Hashmap.find K.map v
            let pp fmt v = Format.pp_print_string fmt (to_string v)
          end
        include M
        include Containers.Make(M)
        let to_int t = t
      end
      
    type var = Var.t
    type var_container = Var.Set.t
      
    let get_limit exist = if exist then !K.min_var else !K.max_var
    let get_diff exist = if exist then (-1) else 1

    let present v = Int.Hashmap.mem K.map v
    let name_present n = Strng.Hashmap.mem K.inv_map n

    let get_var n = Strng.Hashmap.find K.inv_map n

    let to_ints vs = Var.Set.map_to Int.Set.add Int.Set.empty Var.to_int vs

    let is_exist_var v = (not (is_unnamed v)) && v<0
    let is_free_var v = (not (is_unnamed v)) && v>0
    let is_valid_var v exist =
      exist && is_exist_var v || not exist && is_free_var v

    let is_exist_name n = n.[(String.length n)-1] = '\''
    let is_free_name n = not (is_exist_name n)
    let is_valid_name v exist =
      exist && is_exist_name v || not exist && is_free_name v

    let mk_var name exist =
      assert (is_valid_name name exist);
      if name_present name then
        let v = get_var name in assert (is_valid_var v exist) ; v
      else
        let v = (get_diff exist) + (get_limit exist) in
        assert (not (present v) && not (name_present name)) ;
        assert
          (is_exist_var v && is_exist_name name ||
           is_free_var v && is_free_name name);
        Int.Hashmap.add K.map v name ;
        Strng.Hashmap.add K.inv_map name v ;
        K.max_var := max !K.max_var v ;
        K.min_var := min !K.min_var v ;
        v

    let fresh_varname exist =
      let suffix = if exist then "'" else "" in
      let idx = ref 0 in
      let base = ref "a" in
      let gen_name () =
        !base ^
        (if !idx = 0 then "" else string_of_int !idx) ^
        suffix in
      let name = ref (gen_name ()) in
      let letter = ref 'a' in
      while name_present !name && !letter < 'z' do
        letter := char_of_int (1 + (int_of_char !letter)) ;
        base := Lib.string_of_char !letter ;
        name := gen_name ()
      done ;
      if not (name_present !name) then !name else
      begin
        base := K.default_varname exist ;
        idx := 1;
        name := gen_name () ;
        while name_present !name do
          incr idx ;
          name := gen_name ()
        done ;
        assert (not (name_present !name)) ;
        !name
      end

    let fresh_var m exist =
      let limit = abs (get_limit exist) in
      let i = m + (get_diff exist) in
      if abs i <= limit then i else mk_var (fresh_varname exist) exist

    let rec fresh_vars m i exist = match i with
      | 0 -> []
      | n ->
        let v = fresh_var m exist in
        v::(fresh_vars (m + (get_diff exist)) (n-1) exist)

    let bound s exist = if exist then min 0 (Var.Set.min_elt s) else max 0 (Var.Set.max_elt s)
    let fresh_evar s = let m = if Var.Set.is_empty s then 0 else bound s true in fresh_var m true
    let fresh_fvar s = let m = if Var.Set.is_empty s then 0 else bound s false in fresh_var m false
    let fresh_evars s n = let m = if Var.Set.is_empty s then 0 else bound s true in fresh_vars m n true
    let fresh_fvars s n = let m = if Var.Set.is_empty s then 0 else bound s false in fresh_vars m n false
    
    module Subst =
      struct
        type t = Var.t Var.Map.t
        type var = Var.t
        type var_container = Var.Set.t
        let empty = Var.Map.empty
        let singleton x y = Var.Map.add x y empty
        let of_list = Var.Map.of_list
        let pp = Var.Map.pp Var.pp
        let to_string = Var.Map.to_string Var.to_string
        let apply theta v =
          if not (is_unnamed v) && Var.Map.mem v theta then Var.Map.find v theta else v
        let avoid vars subvars =
          let allvars = Var.Set.union vars subvars in
          let (exist_vars, free_vars) =
            Pair.map Var.Set.elements (Var.Set.partition is_exist_var subvars) in
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
            (fun x y -> is_free_var x && (is_unnamed y || is_free_var y))
            theta
      end
  end
