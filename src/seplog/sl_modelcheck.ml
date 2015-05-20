open Util
open Lib
open MParser
open Symbols

module List = Blist

module Var :
  sig
    include BasicType with type t = Sl_term.t
    include CTsig 
      with type Set.elt = t
      with type Map.key = t
      with type Hashmap.key = t
      with type MSet.elt = t
      with type FList.t = t list
    val of_term : Sl_term.t -> t
    exception Not_variable
    val parse : (t, 'a) MParser.t
  end
    =
  struct
    module T =
      struct
        type t = Sl_term.t
        let equal = Sl_term.equal
        let compare = Sl_term.compare
        let hash = Sl_term.hash
        let pp = Sl_term.pp
        let to_string = Sl_term.to_string
      end
    include T
    include ContaineriseType(T)
    exception Not_variable
    let of_term t = if (Sl_term.is_var t) then t else raise Not_variable
    let parse st = (Sl_term.parse |>> (fun t -> of_term t)) st
  end
  
module ModelChecker =
  struct

    module type S = 
      sig
        
        module Location :
          sig
            include NaturalType
            include CTsig 
              with type Set.elt = t
              with type Map.key = t
              with type Hashmap.key = t
              with type MSet.elt = t
              with type FList.t = t list
          end
        module Scalar : NaturalType
        module Value : 
          sig
            include NaturalType
            include CTsig 
              with type Set.elt = t
              with type Map.key = t
              with type Hashmap.key = t
              with type MSet.elt = t
              with type FList.t = t list
            val mk_loc_val : Location.t -> t
            val mk_scalar_val : Scalar.t -> t
            val nil : t
          end
        
        module Heap :
          sig
            
            include BasicType
            
            module Parser :
              sig                
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (Value.t, 'a) MParser.t 
                    val parse_location : (Location.t, 'a) MParser.t 
                  end) : S
              end
            
          end

        module Stack :
          sig
            
            include BasicType

            module Parser :
              sig
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig val parse_scalar : (Value.t, 'a) MParser.t end) : S
              end
            
          end
          
        type model = Stack.t * Heap.t
          
        val mk_model_parser : 
          (((Stack.t, 'a) MParser.t) * ((Heap.t, 'a) MParser.t)) -> (model, 'a) MParser.t
        val model_of_string : (Stack.t * Heap.t, unit) MParser.t -> string -> model 
        
        val setup_defs : Sl_defs.t -> unit
        
        exception Defs_not_initialised
        
        val check_model : (Sl_heap.t * model) -> bool
        
      end
      
    module type ValueSig =
      sig
        module HeapLocation : NaturalType
        module ScalarValue : NaturalType
        val pp_nil : Format.formatter -> unit
      end
  
    module Make (Sig : ValueSig) : S 
        with type Location.t = Sig.HeapLocation.t
        with type Scalar.t = Sig.ScalarValue.t
        =
      struct
        
        module Location = 
          struct
            include Sig.HeapLocation
            include ContaineriseType(Sig.HeapLocation)
          end
        module Scalar = Sig.ScalarValue
        
        module Value =
          struct
            module T =
              struct
                type t =
                  | Nil
                  | Location of Location.t
                  | Scalar of Scalar.t
                let compare v v' = 
                  match (v, v') with
                  | (Nil, Nil) -> 0
                  | (Location(_), Nil) -> 1
                  | (Location(l), Location(l')) -> Sig.HeapLocation.compare l l'
                  | (Scalar(v), Scalar(v')) -> Sig.ScalarValue.compare v v'
                  | (Scalar(_), _) -> 1
                  | (_, _) -> -1
                let equal v v' =
                  match (v, v') with
                  | (Nil, Nil) -> true
                  | (Location(l), Location(l')) -> Sig.HeapLocation.equal l l'
                  | (Scalar(v), Scalar(v')) -> Sig.ScalarValue.equal v v'
                  | (_, _) -> false
                let hash v = Hashtbl.hash v
                let pp fmt = function
                  | Nil -> Sig.pp_nil fmt
                  | Location(l) -> Sig.HeapLocation.pp fmt l
                  | Scalar(v) -> Sig.ScalarValue.pp fmt v
                let to_string v = mk_to_string pp v
                let zero = Nil
                let succ = function
                  | Nil -> Location(Sig.HeapLocation.zero)
                  | Location(l) -> Location(Sig.HeapLocation.succ l)
                  | Scalar(v) -> Scalar(Sig.ScalarValue.succ v)
              end
            include T
            include ContaineriseType(T)
            let mk_loc_val l = Location(l)
            let mk_scalar_val v = Scalar(v)
            let nil = zero
          end
          
        module Heap = 
          struct 
            type t = (Value.FList.t) Location.Map.t
            
            type domain = Location.Set.t
            
            let compare h h' = Location.Map.compare Value.FList.compare h h'
            let equal h h' = Location.Map.equal Value.FList.equal h h'
						let hash h = Hashtbl.hash h
            let pp fmt h = 
              Format.fprintf fmt "@[[@ ";
              Location.Map.iter 
                (fun k v -> Format.fprintf fmt "%a%s(%a),@ " 
                  Location.pp k symb_mapsto.sep 
                  (Blist.pp pp_commasp Value.pp) v)
                h;
              Format.fprintf fmt "]@]"
            let to_string h = mk_to_string pp h
            
            let get_all_vals h = List.fold_left
              (fun vs (l, vs') -> 
                Value.Set.add 
                  (Value.mk_loc_val l) 
                  (Value.Set.union vs (Value.Set.of_list vs')))
              Value.Set.empty
              (Location.Map.bindings h)
            
            module Parser =
              struct
                
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (Value.t, 'a) MParser.t 
                    val parse_location : (Location.t, 'a) MParser.t 
                  end) : S =
                  struct
                    
                    let parse st = (
                        Tokens.squares ( 
                        Tokens.comma_sep (
                        T.parse_location >>= (fun l ->
                        parse_symb symb_mapsto >>
                        Tokens.parens (Tokens.comma_sep T.parse_scalar) |>> 
                        (fun vs -> (l, vs)))) |>> 
                        (fun cells -> Location.Map.of_list cells))) st
                    let of_string s = handle_reply (MParser.parse_string parse s ()) 
                    
                  end
                
              end
            
          end
          
        module Stack = 
          struct
            type t = Value.t Var.Map.t
            
            let compare s s' = Var.Map.compare Value.compare s s'
            let equal s s' = Var.Map.equal Value.equal s s'
						let hash s = Hashtbl.hash s
            let pp fmt h = 
              Format.fprintf fmt "@[[@ ";
              Var.Map.iter 
                (fun k v -> Format.fprintf fmt "%a%s%a,@ " 
                  Sl_term.pp k symb_mapsto.sep Value.pp v)
                h;
              Format.fprintf fmt "]@]"
            let to_string s = mk_to_string pp s
            
            let get_all_vals s = 
              let bindings = Var.Map.bindings s in
              let vals = List.map (fun (_, v) -> v) bindings in
              Value.Set.of_list vals
              
            let empty : t = Var.Map.empty
            
            let of_term_bindings bindings =
              let bindings = List.filter
                (fun (t, v) -> not ((Sl_term.is_nil t) && (Value.equal v Value.nil)))
                bindings in
              Option.map 
                Var.Map.of_list 
                (Option.pred 
                  (List.for_all (fun (t,_) -> Sl_term.is_var t)) 
                  bindings)
            
            let consistent s s' =
              let clashes x v = 
                Var.Map.mem x s' && not (Value.equal v (Var.Map.find x s')) in
              let clash_map = Var.Map.filter clashes s in
              Var.Map.is_empty clash_map
            
            let merge s s' = 
              let merge_f x v v' = match (v, v') with
                | (None, None) -> None
                | (None, v) -> v
                | (v, None) -> v
                | (Some(v), Some(v')) ->
                    if Value.equal v v' then Some(v)
                    else raise (invalid_arg ("Stacks are not consistent in _
                      the value of " ^ (Var.to_string x) ^ "!")) in
              Var.Map.merge merge_f s s'
            
            let satisfies (eqs, deqs) s = 
              let () = debug (fun _ -> "checking stack " ^ (to_string s)) in
              List.for_all
                (fun (t, t') -> 
                  let b =
                  (Sl_term.is_nil t && Sl_term.is_nil t') ||
                  (Sl_term.is_var t && not (Var.Map.mem t s)) ||
                  (Sl_term.is_var t' && not (Var.Map.mem t' s)) ||
                  (Sl_term.is_nil t && 
                    Value.equal Value.nil (Var.Map.find t' s)) ||
                  (Sl_term.is_nil t' &&
                    Value.equal Value.nil (Var.Map.find t s)) ||
                  (Value.equal (Var.Map.find t s) (Var.Map.find t' s)) in
                  let () = debug (fun _ ->
                    "does " ^ 
                    (if b then "" else "not ") ^ "satisfy equality " ^ 
                    (Sl_tpair.to_string (t, t'))) in
                  b)
                (Sl_uf.bindings eqs)
                  &&
              Sl_deqs.for_all
                (fun (t, t') -> 
                  let b =
                  (Sl_term.is_var t && not (Var.Map.mem t s)) ||
                  (Sl_term.is_var t' && not (Var.Map.mem t' s)) ||
                  (Sl_term.is_nil t && (Sl_term.is_var t') && 
                    not (Value.equal Value.nil (Var.Map.find t' s))) ||
                  (Sl_term.is_var t && (Sl_term.is_nil t') && 
                    not (Value.equal Value.nil (Var.Map.find t s))) ||
                  (Sl_term.is_var t && (Sl_term.is_var t') &&
                    not (Value.equal (Var.Map.find t s) (Var.Map.find t' s))) in
                  let () = debug (fun _ ->
                    "does " ^ 
                    (if b then "" else "not ") ^ "satisfy disequality " ^ 
                    (Sl_tpair.to_string (t, t'))) in
                  b)
                (deqs)                  
            
            (* precondition:  satisfies (eqs, deqs) s            *)
            (* precondition:  satisfies (eqs, deqs) s'           *)
            (* precondition:  consistent s s'                    *)
            (* postcondition: satisfies (eqs, deqs) (merge s s') *)
            let cross_satisfies (eqs, deqs) s s' = 
              List.for_all
                (fun (t, t') ->
                  Sl_term.is_nil t || Sl_term.is_nil t' ||
                  ((not (Var.Map.mem t s)) && not (Var.Map.mem t s')) ||
                  ((not (Var.Map.mem t' s)) && not (Var.Map.mem t' s')) ||
                  ((Var.Map.mem t s) && (Var.Map.mem t s')) ||
                  ((Var.Map.mem t' s) && (Var.Map.mem t' s')) ||
                  ((Var.Map.mem t s) && 
                    (Value.equal (Var.Map.find t s) (Var.Map.find t' s'))) ||
                  ((Var.Map.mem t s') && 
                    (Value.equal (Var.Map.find t s') (Var.Map.find t' s))))
                (Sl_uf.bindings eqs)
                  &&
              Sl_deqs.for_all
                (fun (t, t') ->
                  Sl_term.is_nil t || Sl_term.is_nil t' ||
                  ((not (Var.Map.mem t s)) && not (Var.Map.mem t s')) ||
                  ((not (Var.Map.mem t' s)) && not (Var.Map.mem t' s')) ||
                  ((Var.Map.mem t s) && (Var.Map.mem t s')) ||
                  ((Var.Map.mem t' s) && (Var.Map.mem t' s')) ||
                  ((Var.Map.mem t s) && 
                    not (Value.equal (Var.Map.find t s) (Var.Map.find t' s'))) ||
                  ((Var.Map.mem t s') && 
                    not (Value.equal (Var.Map.find t s') (Var.Map.find t' s))))
                (deqs)
            
            module Parser =
              struct
                
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (Value.t, 'a) MParser.t 
                  end) : S =
                  struct
                    
                    let parse st = (
                        Tokens.squares ( 
                        Tokens.comma_sep (
                        Var.parse >>= (fun x ->
                        parse_symb symb_mapsto >>
                        T.parse_scalar |>> (fun v -> (x, v)))) |>> 
                        (fun ps -> Var.Map.of_list ps))) st
                    let of_string s = handle_reply (MParser.parse_string parse s ()) 
                    
                  end
                
              end
              
          end
          
        type model = Stack.t * Heap.t
            
        let mk_model_parser (parse_stack, parse_heap) st = 
          (Tokens.parens (
            parse_stack >>= (fun s ->
            (parse_symb symb_comma) >>
            parse_heap |>> (fun h -> (s, h))))) st
              
        let model_of_string parse s = handle_reply (MParser.parse_string parse s ()) 
        
        module Interpretant = MakeComplexType(PairTypes(Value.FList)(Heap))
        
        module Interpretation :
          sig
            val mk : 
              (Sl_defs.t * Interpretant.t) 
                -> Interpretant.Set.t Sl_predsym.Map.t
          end
            =
          struct

            module InterpretantBase = MakeComplexType(PairTypes(Value.FList)(Location.Set))
            module BaseSetPair = PairTypes(InterpretantBase.Set)(InterpretantBase.Set)
            
            module SymHeapHash = Hashtbl.Make(Sl_heap)
            module SymHeapHashPrinter = HashtablePrinter.Make(SymHeapHash)
            module ModelBase = MakeComplexType(PairTypes(Stack)(Location.Set))
            
            module T =
              struct
                type t = BaseSetPair.t Sl_predsym.Map.t
                let equal x y =
                  let binding_eq (p, (xs, xs')) (p', (ys, ys')) =
                    Sl_predsym.equal p p' &&
                    InterpretantBase.Set.equal xs ys &&
                    InterpretantBase.Set.equal xs' ys' in 
                  List.equal 
                    binding_eq
                    (Sl_predsym.Map.bindings x)
                    (Sl_predsym.Map.bindings y)
              end
            include T
            include Fixpoint(T)
            
            let empty_base = InterpretantBase.Set.empty
            
            let init_empty defs empty_val =
              List.fold_left
                (fun base def -> Sl_predsym.Map.add 
                  (Sl_preddef.predsym def) 
                  empty_val 
                  base)
                Sl_predsym.Map.empty
                (Sl_defs.to_list defs)
              
            let decorate h x = 
              let f (s, s') =
                let subst_heap bs =
                  let subst_heap (vs, ls) = 
                    let h' = Location.Map.filter (fun l _ -> Location.Set.mem l ls) h in
                    (vs, h') in
                  let ls = List.map subst_heap (InterpretantBase.Set.to_list bs) in
                  Interpretant.Set.of_list ls in 
                Interpretant.Set.union (subst_heap s) (subst_heap s') in
              Sl_predsym.Map.map f x
              
            let add_spares n vs = 
              let rec add n v vs = match n with
                | n when n <= 0 -> vs
                | _ -> 
                  let v' = Value.succ v in
                  let vs' = Value.Set.add v' vs in
                  add (n-1) v' vs' in
              add n (Value.Set.max_elt vs) vs
              
            (** 
              This function creates a hashtable which stores a set of model bases      
              for each inductive rule in the definition set that is both consistent
              and contains some number (> 0) of points-to formula atoms. These
              models are the valid interpretations of the entire set of points-to
              atoms in each inductive rule body. The hastable is keyed on a symbolic
              heap formula.
                i.e. We abstract the points-to set for each rule and compute its 
              interpretation only once before starting the fixpoint computation, 
              and make it quickly accessible using a hash table.
              
              Notes:
               1. [all_ptos_itpts] is a map containing all the interpretant bases
                  of the singleton subheaps of [h] keyed on size of the heap cell 
                  being pointed to. This allows easy identification of only those 
                  subheaps relevant to any given points-to formula atom.
               2. We calculate more or less the precise size we will need for
                  the hashtable in [num_buckets]; this is done by counting the
                  number of inductive rule bodies that are both consistent and
                  have a greater than zero number of points-to formula atoms. 
                    Note that this is, in practice, a precise bound since it is
                  unlikely that there will be exactly duplicated inductive rule
                  bodies.
             **)
            let mk_ptos_base defs h =
              let all_ptos_itpts =
                let f = fun loc cell ptos ->
                  let cell_size = List.length cell in
                  let base =
                    if Int.Map.mem cell_size ptos then
                      Int.Map.find cell_size ptos
                    else InterpretantBase.Set.empty in
                  let pto = 
                    ((Value.mk_loc_val loc)::cell, Location.Set.singleton loc) in
                  let base = InterpretantBase.Set.add pto base in
                  Int.Map.add cell_size base ptos in
                Location.Map.fold f h Int.Map.empty in
              let () = debug (fun _ -> Int.Map.to_string InterpretantBase.Set.to_string all_ptos_itpts) in
              let num_buckets = 
                let test_and_incr n rl =
                  let (body, _) = Sl_indrule.dest rl in 
                  let inc = 
                    if (Sl_heap.inconsistent body) then 0
                    else let (_,_, ptos, _) = Sl_heap.dest body in 
                      min 1 (Sl_ptos.cardinal ptos) in
                  n + inc in
                Sl_defs.rule_fold test_and_incr 0 defs in
              let base = SymHeapHash.create num_buckets in 
              let calc_abstractions rl =
                let (body, _) = Sl_indrule.dest rl in
                let (eqs, deqs, ptos, _) = Sl_heap.dest body in 
                let constraints = (eqs, deqs) in
                if (not (Sl_heap.inconsistent body)) && 
                   (Sl_ptos.cardinal ptos > 0) then
                begin
                  let itpts =
                    let gen_itpts (t, ts) itpts =
                      let pto_models =
                        let pto_itpts = 
                          let cell_size = List.length ts in
                          if Int.Map.mem cell_size all_ptos_itpts then
                            Int.Map.find cell_size all_ptos_itpts
                          else InterpretantBase.Set.empty in
                        let add_pto_model (vs, ls) models =
                          match (Stack.of_term_bindings (List.combine (t::ts) vs)) with
                          | None -> models
                          | Some(stack) ->
                              if Stack.satisfies constraints stack then
                                ModelBase.Set.add (stack, ls) models
                              else models in
                        InterpretantBase.Set.fold add_pto_model pto_itpts ModelBase.Set.empty in
                      let add_pto_models (s, ls) itpts =
                        let test_and_merge (s', l) itpts =
                          if (Location.Set.disjoint ls l) && 
                             (Stack.consistent s s') &&
                             (Stack.cross_satisfies constraints s s') then
                            let new_mdl =
                              let new_stack = Stack.merge s s' in
                              let new_heap_spt = Location.Set.union ls l in 
                              (new_stack, new_heap_spt) in
                            ModelBase.Set.add new_mdl itpts
                          else itpts in
                        ModelBase.Set.fold test_and_merge pto_models itpts in
                      ModelBase.Set.fold add_pto_models itpts ModelBase.Set.empty in
                    let start = ModelBase.Set.singleton (Stack.empty, Location.Set.empty) in
                    Sl_ptos.fold gen_itpts ptos start in
                  SymHeapHash.add base body itpts 
                end in
              let () = Sl_defs.rule_iter calc_abstractions defs in
              base
              
            let mk_generator (defs, (vs, h)) =
              let valset = Value.Set.union
                (Value.Set.of_list vs)
                (Heap.get_all_vals h) in
              let valset = 
                let max_vars_of_defs = 
                  let update_max m rl = 
                    max m (Sl_term.Set.cardinal (Sl_indrule.vars rl)) in
                  Sl_defs.rule_fold update_max 0 defs in
                add_spares max_vars_of_defs valset in
              let valset = Value.Set.add Value.nil valset in
              let ptos_base = mk_ptos_base defs h in
              let () = debug (fun _ -> Value.Set.to_string valset) in
              let () = debug (fun _ -> SymHeapHashPrinter.to_string Sl_heap.to_string ModelBase.Set.to_string ptos_base) in
              let generator itp = 
                (* For each rule in the definition set, generate a new interpretation *)
                (* containing the fresh interpretants; then combine with the old interpretation *)
                let rule_gen new_itp rl = new_itp
                  in
                let new_itp = Sl_defs.rule_fold rule_gen (init_empty defs empty_base) defs in
                try
                  Sl_predsym.Map.of_list
                    (List.map2
                      (fun (p, (xs, ys)) (p', zs) ->
                        if not (Sl_predsym.equal p p') then
                          failwith ("Something has gone wrong: " 
                            ^ (Sl_predsym.to_string p) ^ " and " 
                            ^ (Sl_predsym.to_string p') ^ " do not match!")
                        else
                          (p, (InterpretantBase.Set.union xs ys, zs)) 
                        )
                      (Sl_predsym.Map.bindings itp)
                      (Sl_predsym.Map.bindings new_itp))
                  with Invalid_argument(s) -> 
                    failwith (
                      "Something wrong: interpretations map different numbers of predicates!\n" 
                      ^ (Printexc.to_string (Invalid_argument(s))) ^ "\n"
                      ^ (Printexc.get_backtrace ())) 
                in
              generator 
              
            let mk (defs, (vs, h)) =
              let generator = mk_generator (defs, (vs, h)) in
              let start_itp = init_empty defs (empty_base, empty_base) in
              let base = fixpoint generator start_itp in
              decorate h base
            
          end
          
        let sl_defs = ref (None : Sl_defs.t option)
          
        exception Defs_not_initialised
          
        let setup_defs defs = sl_defs := Some defs
        
        let relevant_defs all_defs f = 
          let ident_set f = Sl_predsym.Set.of_list (Sl_predsym.MSet.to_list (Sl_heap.idents f)) in
          let iterate preds =
            let add_preds pred preds = 
              let rules = Sl_defs.get_def pred all_defs in
              let add_preds_from_rule preds rule =
                let (body, _) = Sl_indrule.dest rule in
                let new_preds = ident_set body in
                Sl_predsym.Set.union preds new_preds in
              Blist.foldl add_preds_from_rule preds rules in
            Sl_predsym.Set.fold add_preds preds preds in
          let relevant_preds = Sl_predsym.Set.fixpoint iterate (ident_set f) in
          Sl_predsym.Set.fold
            (fun pred defs -> Sl_defs.add (Sl_preddef.mk ((Sl_defs.get_def pred all_defs), pred)) defs)
            relevant_preds
            Sl_defs.empty
            
        let check_model (sh, (stk, h)) =
          match !sl_defs with
          | None -> raise Defs_not_initialised
          | Some(defs) ->
          let defs = relevant_defs defs sh in
          let defs = Sl_defs.of_formula defs [sh] in
          let new_def = List.hd (Sl_defs.to_list defs) in
          let new_predsym = Sl_preddef.predsym (new_def) in
          let vals = 
            let rl = 
              let rls = Sl_preddef.rules new_def in
              if List.length rls == 1 then
                List.hd rls
              else
                (* Sanity check *)
                failwith "Unexpected number of clauses in new definition" in
            let formals = Sl_indrule.formals rl in
            try
              List.map
                (fun x -> 
                  try Var.Map.find x stk
                  with Not_found -> invalid_arg (Sl_term.to_string x))
                formals
            with Invalid_argument(var) -> 
              failwith ("No mapping found for " ^ var ^ " in provided stack") in
          let () = debug (fun _ -> Sl_defs.to_string defs) in
          let () = debug (fun _ -> Stack.to_string stk) in
          let () = debug (fun _ -> Heap.to_string h) in
          let () = debug (fun _ -> Sl_preddef.to_string new_def) in
          let () = debug (fun _ -> Value.FList.to_string vals) in          
          let interp = Interpretation.mk (defs, (vals, h)) in 
          Interpretant.Set.mem
            (vals, h)
            (Sl_predsym.Map.find new_predsym interp) 
        
      end
            
  end
  
open ModelChecker

module IntSig : ValueSig 
  with type HeapLocation.t = NatType.t
  with type ScalarValue.t = NatType.t
    =
  struct
    module HeapLocation = NatType
    module ScalarValue = NatType
    let pp_nil fmt = IntType.pp fmt 0
  end
  
module IntSigModelChecker = Make(IntSig)
open IntSigModelChecker
  
module IntSigParser =
  struct
    let parse_location = Tokens.decimal
    let parse_scalar st = 
      (Tokens.decimal |>> (fun v ->
        if v == 0 then Value.zero else (Value.mk_loc_val v))) st
  end
  
module StackParser = Stack.Parser.Make(IntSigParser)
module HeapParser = Heap.Parser.Make(IntSigParser)

let model_parser st = (mk_model_parser (StackParser.parse, HeapParser.parse)) st

let defs_path = ref "examples/sl.defs"
let str_model = ref ""
let str_symheap = ref ""

let usage = 
  (
    "usage: " ^ 
    Sys.argv.(0) ^ 
    " [-D <file>] -M <string> -F <string>"
    )

let speclist = [
    ("-D", Arg.Set_string defs_path, 
      ": read inductive definitions from <file>, default is " ^ !defs_path);
    ("-d", Arg.Set do_debug,": print debug messages");
    ("-s", Arg.Set Stats.do_statistics,": print statistics");
    ("-M", Arg.Set_string str_model, ": <string> model to be checked");
    ("-F", Arg.Set_string str_symheap, ": <string> symbolic heap to check against");
  ]

let die msg =
  print_endline msg ;
  print_endline (Arg.usage_string speclist usage) ;
  exit 1
  
let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if !str_model="" then die "-M must be specified." ;
  if !str_symheap="" then die "-F must be specified." ;
  let sh = Sl_heap.of_string !str_symheap in
  let () = setup_defs (Sl_defs.of_channel (open_in !defs_path)) in
  let model = model_of_string model_parser !str_model in
  begin
    Stats.reset () ;
    Stats.Gen.call () ;
    let call () = check_model (sh, model) in
    let res = call () in
    Stats.Gen.end_call () ;
    if !Stats.do_statistics then Stats.gen_print ();
    if res then
      print_endline("Model verified")
    else
      print_endline("Not a satisfying model!")
  end

