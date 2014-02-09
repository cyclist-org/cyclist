(** Module for handling variable name generation. 
    Variables are represented by integers and associated with string names. 
    - An existential variable name {i must} end in ['].
    - A universal variable name must {i not} end in ['].
    - Zero represents nothing and will trigger an exception if used as an 
    argument.
    
    Invariants: 
    - Any variable passed to these functions must have been obtained
    by a call to [mk_*_var] or to [fresh_*var*], otherwise an exception will occur.
    - User defined names must obey the restriction about ['] or an exception will
    occur. 
*)

(** Retrieves the name of the variable. *)
val to_string : int -> string

val is_var : int -> bool
val is_exist_var : int -> bool
val is_univ_var : int -> bool

(** checks whether name ends with ' *)
val is_exist_name : string -> bool

(** checks whether name does *not* end with ' *)
val is_univ_name : string -> bool

(** Allocate a fresh universal variable associated to the variable name passed. 
    If the name has been used previously, the integer allocated before is 
    returned. *)
val mk_univ_var : string -> int

(** Allocate a fresh existential variable associated to the variable name passed. 
    If the name has been used previously, the integer allocated before is 
    returned. *)
val mk_exist_var : string -> int

(** Return an existentially quantified variable {i not} 
    in the set of variables provided as argument. 
    The result will have been previously allocated if possible, otherwise a 
    completely fresh variable is returned via [mk_*_var] whose name is generated
    automatically. *)
val fresh_evar : Util.Int.Set.t -> int

(** Return a universally quantified variable {i not} 
    in the set of variables provided as argument. 
    The result will have been previously allocated if possible, otherwise a 
    completely fresh variable is returned via [mk_*_var] whose name is generated
    automatically. *)
val fresh_uvar : Util.Int.Set.t -> int

(** Return a list of existentially quantified variables of the requested length
    obeying the restrictions for [fresh_*var]. *)
val fresh_evars : Util.Int.Set.t -> int -> int list

(** Return a list of universally quantified variables of the requested length
    obeying the restrictions for [fresh_*var]. *)
val fresh_uvars : Util.Int.Set.t -> int -> int list