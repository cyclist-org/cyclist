open Util
open Lib
open MParser
open Symbols

module List = Blist

module Var :
  sig
    include BasicType
    module Set : OrderedContainer with type elt = t
    module Map : OrderedMap with type key = t
    val of_term : Sl_term.t -> t
    val to_term : t -> Sl_term.t
    val parse : (t, 'a) MParser.t
  end
    =
  struct
    include Sl_term
    let of_term t = assert (Sl_term.is_var t) ; t
    let to_term t = t
    let parse st = (Sl_term.parse |>> (fun t -> of_term t)) st
  end

module type ParserSig =
  sig
    type t
    val parse : (t, 'a) MParser.t
    val of_string : string -> t
  end

module type S =
  sig

    module Location : NaturalType
    module Scalar : NaturalType
    module Value :
      sig
        include NaturalType
        val mk_loc_val : Location.t -> t
        val mk_scalar_val : Scalar.t -> t
        (* val nil : t  -- FIXME: unused *)
    end

    module Heap :
      sig
        include BasicType
        module MakeParser (T : sig
            val parse_scalar : (Value.t, 'a) MParser.t
            val parse_location : (Location.t, 'a) MParser.t
          end) : ParserSig with type t = t
        val size : t -> int
      end

    module Stack :
      sig
        include BasicType
        module MakeParser (T : sig
            val parse_scalar : (Value.t, 'a) MParser.t
          end) : ParserSig with type t = t
      end

    type model = Stack.t * Heap.t

    val mk_model_parser :
      (((Stack.t, 'a) MParser.t) * ((Heap.t, 'a) MParser.t)) -> (model, 'a) MParser.t
    val model_of_string : (Stack.t * Heap.t, unit) MParser.t -> string -> model
    val check_model : Sl_defs.t -> (Sl_heap.t * model) -> bool
  end

module type ValueSig =
  sig
    module HeapLocation : NaturalType
    module ScalarValue : NaturalType
    val pp_nil : Format.formatter -> unit
  end

module Make (Sig : ValueSig)
  (* : S                                         *)
  (*   with type Location.t = Sig.HeapLocation.t *)
  (*   with type Scalar.t = Sig.ScalarValue.t    *)
    =
  struct

    let max_hashset_size = ref 15485863

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
            let rec hash = function
              | Nil -> 11
              | Location(l) -> max_int land (19 * (Location.hash l) + 1)
              | Scalar(v) -> max_int land (19 * (Scalar.hash v) + 2)
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
        let hash h = Location.Map.hash Value.FList.hash h
        let pp fmt h =
          Format.fprintf fmt "@[[@ ";
          Location.Map.iter
            (fun k v -> Format.fprintf fmt "%a%s(%a),@ "
              Location.pp k symb_mapsto.sep
              (Blist.pp pp_commasp Value.pp) v)
            h;
          Format.fprintf fmt "]@]"
        let to_string h = mk_to_string pp h

        let get_all_vals h =
          Location.Map.fold
            (fun l lvs vs ->
              List.fold_left
                (fun vs' x -> Value.Set.add x vs')
                (Value.Set.add (Value.mk_loc_val l) vs)
                lvs)
            h
            Value.Set.empty

        let size h = List.length (Location.Map.bindings h)

        type heap  = t (* used to get aroung cyclicity of type t = t below *)
        module MakeParser (T : sig
            val parse_scalar : (Value.t, 'a) MParser.t
            val parse_location : (Location.t, 'a) MParser.t
          end)  =
          struct
            type t = heap

            let parse st = (
                Tokens.squares (
                Tokens.comma_sep (
                T.parse_location >>= (fun l ->
                parse_symb symb_mapsto >>
                (optional parse_ident) >>
                Tokens.parens (Tokens.comma_sep T.parse_scalar) |>>
                (fun vs -> (l, vs)))) |>>
                (fun cells -> Location.Map.of_list cells))) st
            let of_string s = handle_reply (MParser.parse_string parse s ())

          end
      end

    module Stack =
      struct
        type t = Value.t Var.Map.t

        let compare s s' = Var.Map.compare Value.compare s s'
        let equal s s' = Var.Map.equal Value.equal s s'
        let hash s = Var.Map.hash Value.hash s
        let pp fmt h =
          Format.fprintf fmt "@[[@ ";
          Var.Map.iter
            (fun k v -> Format.fprintf fmt "%a%s%a,@ "
              Var.pp k symb_mapsto.sep Value.pp v)
            h;
          Format.fprintf fmt "]@]"
        let to_string s = mk_to_string pp s

        let vars stack =
          Var.Map.fold
            (fun v _ s -> Var.Set.add v s)
            stack
            Var.Set.empty

        let get_all_vals stack =
          Var.Map.fold
            (fun _ v s -> Value.Set.add v s)
            stack
            Value.Set.empty

        let empty : t = Var.Map.empty

        let of_term_bindings bindings =
          let bindings = List.filter
            (fun (t, v) -> not (Sl_term.is_nil t && Value.equal v Value.nil))
            bindings in
          if List.for_all (fun (t,_) -> Sl_term.is_var t) bindings then
              Some (Var.Map.of_list (List.map (fun (x,y) -> (Var.of_term x, y)) bindings))
            else
              None

        let consistent s s' =
          let () = debug (fun _ -> "checking stacks consistent:\n\t" ^ (to_string s) ^ "\n\t" ^ (to_string s')) in
          Var.Map.for_all
            (fun x v ->
              let b =
                not (Var.Map.mem x s') || Value.equal v (Var.Map.find x s') in
              let () = debug (fun _ ->
                (if b then "" else "do not ") ^ "agree on " ^ (Var.to_string x)) in
              b)
            s

        let merge s s' =
          let merge_f x v v' = match (v, v') with
            | (None, None) -> None
            | (None, v) -> v
            | (v, None) -> v
            | (Some(v) as ret, Some(v')) -> assert (Value.equal v v') ; ret in
          Var.Map.merge merge_f s s'

        let satisfies (eqs, deqs) s =
          let () = debug (fun _ -> "checking stack " ^ (to_string s)) in
          Sl_uf.for_all
            (fun t t' ->
              let b =
              (Sl_term.is_nil t && Sl_term.is_nil t') ||
              (Sl_term.is_var t && not (Var.Map.mem (Var.of_term t) s)) ||
              (Sl_term.is_var t' && not (Var.Map.mem (Var.of_term t') s)) ||
              (Sl_term.is_nil t &&
                Value.equal Value.nil (Var.Map.find (Var.of_term t') s)) ||
              (Sl_term.is_nil t' &&
                Value.equal Value.nil (Var.Map.find (Var.of_term t) s)) ||
              (Sl_term.is_var t && Sl_term.is_var t' &&
                Value.equal (Var.Map.find (Var.of_term t) s) (Var.Map.find (Var.of_term t') s)) in
              let () = debug (fun _ ->
                "does " ^
                (if b then "" else "not ") ^ "satisfy equality " ^
                (Sl_tpair.to_string (t, t'))) in
              b)
            eqs
              &&
          Sl_deqs.for_all
            (fun (t, t') ->
              let b =
              (Sl_term.is_var t && not (Var.Map.mem (Var.of_term t) s)) ||
              (Sl_term.is_var t' && not (Var.Map.mem (Var.of_term t') s)) ||
              (Sl_term.is_nil t && (Sl_term.is_var t') &&
                not (Value.equal Value.nil (Var.Map.find (Var.of_term t') s))) ||
              (Sl_term.is_var t && (Sl_term.is_nil t') &&
                not (Value.equal Value.nil (Var.Map.find (Var.of_term t) s))) ||
              (Sl_term.is_var t && (Sl_term.is_var t') &&
                not (Value.equal (Var.Map.find (Var.of_term t) s) (Var.Map.find (Var.of_term t') s))) in
              let () = debug (fun _ ->
                "does " ^
                (if b then "" else "not ") ^ "satisfy disequality " ^
                (Sl_tpair.to_string (t, t'))) in
              b)
            deqs

        (* precondition:  satisfies (eqs, deqs) s            *)
        (* precondition:  satisfies (eqs, deqs) s'           *)
        (* precondition:  consistent s s'                    *)
        (* postcondition: satisfies (eqs, deqs) (merge s s') *)
        let cross_satisfies (eqs, deqs) s s' =
          let () = debug (fun _ -> "checking combination of stacks " ^ (to_string s) ^ " and " ^ (to_string s')) in
          Sl_uf.for_all
            (fun t t' ->
              let b =
              Sl_term.is_nil t || Sl_term.is_nil t' ||
              let t = Var.of_term t in
              let t' = Var.of_term t' in
              ((not (Var.Map.mem t s)) && not (Var.Map.mem t s')) ||
              ((not (Var.Map.mem t' s)) && not (Var.Map.mem t' s')) ||
              ((Var.Map.mem t s) && (Var.Map.mem t s')) ||
              ((Var.Map.mem t' s) && (Var.Map.mem t' s')) ||
              ((Var.Map.mem t s) &&
                (Value.equal (Var.Map.find t s) (Var.Map.find t' s'))) ||
              ((Var.Map.mem t s') &&
                (Value.equal (Var.Map.find t s') (Var.Map.find t' s))) in
              let () = debug (fun _ ->
                "does " ^
                (if b then "" else "not ") ^ "satisfy " ^
                (Sl_term.to_string t) ^ " = " ^ (Sl_term.to_string t')) in
              b)
            eqs
              &&
          Sl_deqs.for_all
            (fun (t, t') ->
              let b =
              Sl_term.is_nil t || Sl_term.is_nil t' ||
              let t = Var.of_term t in
              let t' = Var.of_term t' in
              ((not (Var.Map.mem t s)) && not (Var.Map.mem t s')) ||
              ((not (Var.Map.mem t' s)) && not (Var.Map.mem t' s')) ||
              ((not (Var.Map.mem t s)) && (not (Var.Map.mem t' s))) ||
              ((not (Var.Map.mem t s')) && (not (Var.Map.mem t' s'))) ||
              ((Var.Map.mem t s) && (Var.Map.mem t s')) ||
              ((Var.Map.mem t' s) && (Var.Map.mem t' s')) ||
              ((Var.Map.mem t s) && (Var.Map.mem t' s') &&
                not (Value.equal (Var.Map.find t s) (Var.Map.find t' s'))) ||
              ((Var.Map.mem t s') && (Var.Map.mem t' s) &&
                not (Value.equal (Var.Map.find t s') (Var.Map.find t' s))) in
              let () = debug (fun _ ->
                "does " ^
                (if b then "" else "not ") ^ "satisfy " ^
                (Sl_term.to_string t) ^ " != " ^ (Sl_term.to_string t')) in
              b)
            deqs

        type stack = t
        module MakeParser (T : sig
            val parse_scalar : (Value.t, 'a) MParser.t
          end) =
          struct
            type t = stack
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

    let mk_model_parser (parse_stack, parse_heap) st =
      (Tokens.parens (
        parse_stack >>= (fun s ->
        (parse_symb symb_comma) >>
        parse_heap |>> (fun h -> (s, h))))) st

    let model_of_string parse s = handle_reply (MParser.parse_string parse s ())

    module Interpretant = MakeComplexType(PairTypes(Value.FList)(Heap))

    module SetBase =
      struct
        include Location.Set
        let empty = Location.Set.empty
        let inj h h' =
          assert (Location.Map.for_all (fun k _ -> Location.Map.mem k h) h') ;
          Location.Map.fold (fun l _ ls -> Location.Set.add l ls) h' empty
        let proj h x =
          assert (Location.Set.for_all (fun l -> Location.Map.mem l h) x) ;
          Location.Map.filter (fun k _ -> Location.Set.mem k x) h
        (* TODO: Eta-expand *)
        let disjoint = Location.Set.disjoint
        let union = Location.Set.union
      end

    module BitvBase =
      struct
        let to_bool_list b =
          Bitv_string.fold_right (fun v vs -> v::vs) b []
        (* Functions for implementing BasicType signature *)
        type t = Bitv_string.t
        (* compare, equal and hash are not the "correct" solutions    *)
        (* because they also compare "rubbish" that might be present  *)
        (* in the underlying string that is not actually part ot the  *)
        (* bitvector, however it should not make a difference for our *)
        (* code since all such "rubbish" should be '0' bits.          *)
        let compare b b' =
          String.compare b.Bitv_string.bits b'.Bitv_string.bits
        let equal b b' =
          String.compare b.Bitv_string.bits b'.Bitv_string.bits == 0
        let hash b = Strng.hash (b.Bitv_string.bits)
        let to_string b = Printf.sprintf "bitv(%s)" (Blist.to_string ""
          (fun v -> if v then "1" else "0")
          (to_bool_list b))
        let pp fmt b = Format.fprintf fmt "%s" (to_string b)

        let empty = Bitv_string.create 0 false

        let inj h =
          let cover = Array.make
            (Location.Map.cardinal h)
            (Location.zero, []) in
          let () = List.iteri
            (fun i (l,c) -> Array.set cover i (l,c))
            (Location.Map.bindings h) in
          let rec binary_search ((l,c) as e) min max =
            if max < min then
              invalid_arg ("Not a subheap!")
            else
              let mid = (min + max) / 2 in
              let (l',c') = Array.get cover mid in
              let cmp = Location.compare l l' in
              if cmp < 0 then
                binary_search e min (mid-1)
              else if cmp > 0 then
                binary_search e (mid+1) max
              else
                if Value.FList.equal c c' then mid
                else invalid_arg ("Not a subheap!") in
          let arr_size = Array.length cover in
          fun h' ->
            let b = Bitv_string.create (Array.length cover) false in
            let bs = Location.Map.bindings h' in
            let _ =
              let f i ((l, c) as e) =
                let j = binary_search e i arr_size in
                let () = Bitv_string.unsafe_set b j true in
                j + 1 in
              List.fold_left f 0 bs in
            b

        let proj h b =
          if (Bitv_string.length b) == 0 then Location.Map.empty
          else if not
              ((Location.Map.cardinal h) == (Bitv_string.length b))
            then
              invalid_arg ("Missing locations!")
          else
            let cs = List.mapi
              (fun i (l,c) ->
                Option.mk (Bitv_string.unsafe_get b i) (l,c))
              (Location.Map.bindings h) in
            Location.Map.of_list (Option.list_get cs)

        let disjoint b b' =
          (Bitv_string.length b == 0) || (Bitv_string.length b' == 0) ||
          (Bitv_string.pop (Bitv_string.bw_and b b') == 0)

        let union b b' =
          if (Bitv_string.length b == 0) then b'
          else if (Bitv_string.length b' == 0) then b
          else (Bitv_string.bw_or b b')
      end

    module HeapBase :
      sig
        include BasicType
        val empty : t
        val inj : Heap.t -> Heap.t -> t
        val proj : Heap.t -> t -> Heap.t
        val disjoint : t -> t -> bool
        val union : t -> t -> t
      end
        = BitvBase

    module InterpretantBase = MakeComplexType(PairTypes(Value.FList)(HeapBase))

    let baseSetPair_to_string bp =
      Pair.to_string 
        InterpretantBase.Hashset.to_string 
        InterpretantBase.Hashset.to_string
        bp
    
    module SymHeapHash = Hashtbl.Make(Sl_heap)
    module SymHeapHashPrinter = HashtablePrinter.Make(SymHeapHash)
    module ModelBase = MakeComplexType(PairTypes(Stack)(HeapBase))

    let empty_base () = InterpretantBase.Hashset.create 11

    (** [itp_emp] is the minimal set of model bases of the formula emp,
          i.e. the singleton set containing the model base consisting of
          the empty stack and the empty heap base.
     *)
    let itp_emp () =
      let itp_emp = ModelBase.Hashset.create 11 in
      ModelBase.Hashset.add itp_emp (Stack.empty, HeapBase.empty) ;
      itp_emp

    let init_empty defs =
      Sl_predsym.Map.of_list 
        (List.map
          (fun def -> (Sl_preddef.predsym def, (empty_base (), empty_base ())))
          (Sl_defs.to_list defs))
    
    let decorate h itp =
      let f (ancestors, parents) =
        InterpretantBase.Hashset.left_union ancestors parents in
      Sl_predsym.Map.map f itp

    let add_spares n vs =
      let rec add n v vs =
        if n <= 0 then vs else
        let v' = Value.succ v in
        let vs' = Value.Set.add v' vs in
        add (n-1) v' vs' in
      let max_elt =
        if Value.Set.is_empty vs then Value.zero
        else Value.Set.max_elt vs in
      add n max_elt vs

    (**
      Given a list of terms [ts] which are the formal parameters of
      some atomic spatial formula (predicate or points-to) F, some pure
      [constraints] Pi, and a set of interpretants [itpts] of F,
      [generate_models ts constraints itpts] generates a hashset of
      model bases which represents the interpretation of (Pi : F)
    **)
    let generate_model ts constraints (vs, ls) =
      Option.bind  
        (fun stack -> 
          if Stack.satisfies constraints stack then Some (stack, ls) else None)
        (Stack.of_term_bindings (List.combine ts vs))
      
    let generate_models_ls ts constraints itpts =
      let f acc itpt =
        Option.fold
          (fun mdl acc -> mdl::acc) 
          (generate_model ts constraints itpt) 
          acc in
      List.fold_left f [] itpts 

    let generate_models ts constraints itpts =
      let models =
        ModelBase.Hashset.create (InterpretantBase.Hashset.cardinal itpts) in
      let f itpt =
        Option.iter
          (fun mdl -> ModelBase.Hashset.add models mdl) 
          (generate_model ts constraints itpt )in
      InterpretantBase.Hashset.iter f itpts ;
      models

    (**
      Given some pure [constraints] Pi and two sets of model bases [ms]
      and [ms'] representing the interpretation of two formulas (Pi : F)
      and (Pi : G) respectively, [cross_models constraints ms ms']
      generates the set of model bases that denotes the intepretation of
      (Pi : F * G).
    **)
    
    let cross_model constraints (s, ls) (s', ls') =
      if (HeapBase.disjoint ls ls') &&
         (Stack.consistent s s') &&
         (Stack.cross_satisfies constraints s s') then
        let new_stack = Stack.merge s s' in
        let new_heap_spt = HeapBase.union ls ls' in
        Some (new_stack, new_heap_spt) 
      else
        None

    let cross_models_ls constraints ms ms' =
      let merge acc m =
        let merge_acc acc' m' =
          Option.fold
            (fun new_mdl acc'' -> new_mdl::acc'')
            (cross_model constraints m m') 
            acc' in
        List.fold_left merge_acc acc ms' in
      List.fold_left merge [] ms 
        
    (* Note: I had thought about a more declarative implementation    *)
    (* which first generates the cross product of ms with ms', then   *)
    (* filters out those elements which do not satisfy the guard      *)
    (* condition of the if statement, and then transforms each        *)
    (* remaining element by combining the stacks and heap bases (for  *)
    (* this, I wrote a polymorphic function in the MakeComplexType    *)
    (* functor to calculate the cross product - I have commented out  *)
    (* this function but left it in the codebase in case it is useful *)
    (* in future). Such an implementation is arguably clearer to      *)
    (* understand, but while it is not computationally more expensive *)
    (* in terms of time, it is more expensive in terms of space since *)
    (* it always generates all possible combinations. The implementa- *)
    (* tion below uses an accumulator to only generate the necesasry  *)
    (* number of new model bases. *)
    let cross_models constraints ms ms' =
      (* A bit of an optimisation: if one or the other of the input   *)
      (* model base hashsets is empty then just return it, otherwise  *)
      (* then we have to compute the cross-product. *)
      if ModelBase.Hashset.is_empty ms then ms
      else if ModelBase.Hashset.is_empty ms' then ms'
      else
      (* Hashset.create: we can tweak the initial size of the hashset for performance *)
      let size = min
        !max_hashset_size
        ((ModelBase.Hashset.cardinal ms) * (ModelBase.Hashset.cardinal ms')) in
      let new_mdls = ModelBase.Hashset.create size in
      let merge m =
        let merge_acc m' =
          Option.iter
            (fun new_mdl -> ModelBase.Hashset.add new_mdls new_mdl)
            (cross_model constraints m m') in
        ModelBase.Hashset.iter merge_acc ms' in
      ModelBase.Hashset.iter merge ms ;
      new_mdls

    (* Note: some efficiency savings to be made here possibly along the *)
    (* lines of fusing the operation of [f] with the generation of the *)
    (* set of possible valuations - in the case of when this is called from *)
    (* exs_satisfiable with [f] being passed List.find_some it would be *)
    (* nice to call [test_exn] as soon as a possible valuation is computed *)
    (* and then stop the generation of further valuations and return *)
    (* immediately. *)
    let valid_extns (eqs, deqs) vs xs s f =
      let mapped_vars = Stack.vars s in
      let (det_extn, still_to_be_mapped) = Var.Set.fold
        (fun x (bndgs, zs) ->
          let y =
            Var.Set.find_opt
              (fun z -> Sl_uf.equates eqs (Var.to_term x) (Var.to_term z))
              mapped_vars in
          match y with
            | None -> (bndgs, zs)
            | Some(y) -> ((x, (Var.Map.find y s))::bndgs, (Var.Set.remove x zs)))
        xs
        ([], xs) in
      let s' = Var.Map.add_bindings det_extn s in
      let equiv_classes =
        let rec add_to_classes t = function
          | [] -> [Var.Set.singleton t]
          | c::cs ->
            let found = Var.Set.exists
              (fun t' -> Sl_uf.equates eqs (Var.to_term t) (Var.to_term t')) c in
            if found then (Var.Set.add t c)::cs
            else c::(add_to_classes t cs) in
        Var.Set.fold add_to_classes still_to_be_mapped [] in
      let valuations = Fun.iter
        (fun acc -> List.flatten
          (Value.Set.fold
            (fun v acc' ->
              (List.map (fun ls -> v::ls) acc)::acc') vs []))
        (List.length equiv_classes)
        [[]] in
        (* let f _ acc =                                   *)
        (*   Value.Set.map_to_list (fun v -> v::acc) vs in *)
        (* List.weave f f List.flatten equiv_classes [] in *)
      let test_extn valuation =
        let ext = List.fold_left2
          (fun bndgs v eq_class -> Var.Set.fold
            (fun x bndgs -> (x, v)::bndgs) eq_class bndgs)
          [] valuation equiv_classes in
        let s' = Var.Map.add_bindings ext s' in
        (* Note: assuming that s satisfies eqs, then by construction *)
        (* so too does s', thus we need only check it satisfies deqs *)
        Option.pred (Stack.satisfies (Sl_uf.empty, deqs)) s' in
      f test_extn valuations

    (**
      [saturate params constraints vs mdls] generates a new set of model
      bases from [mdls] by extending the stacks of each model base in
      [mdls] with mappings to values in [vs] from every universal
      variable either in params or mentioned in [constraints] that is
      not already mapped. Each model base in [mdls] gives rise to a new
      model base for every possible satisfying extension. Thus, every
      model base in [mdls] may give rise to zero or more models in the
      returned set.
    **)
    (* precondition: for all mdl in [mdls] :                          *)
    (*   mdl satisfies [constraints]                                  *)
    (* postcondidition:                                               *)
    (*   for all mdl in [saturate_univs constraints vs mdls] :        *)
    (*     mdl satisfies [constraints]                                *)
    let saturate_univs_one params (eqs, deqs) vs (s, ls) =
      let unmapped_univs = Var.Set.filter
        (fun x -> (Sl_term.is_univ_var (Var.to_term x)) && not (Var.Map.mem x s))
        (Var.Set.union params
          (Var.Set.of_list
            (List.map Var.of_term
              (List.append
                (Sl_term.Set.to_list (Sl_uf.vars eqs))
                (Sl_term.Set.to_list (Sl_deqs.vars deqs)))))) in
      if Var.Set.is_empty unmapped_univs then 
        [(s, ls)]
      else
        let good_stacks = 
          valid_extns (eqs, deqs) vs unmapped_univs s List.map in
        List.fold_left
          (fun acc o ->
            Option.fold
              (fun stk acc' -> (stk, ls)::acc')
              o
              acc
          )    
          []
          good_stacks 
    
    let saturate_univs_ls params (eqs, deqs) vs mdls =
      List.fold_left 
        (fun acc mdl -> 
          let mdls' = saturate_univs_one params (eqs, deqs) vs mdl in
          List.fold_left
            (fun acc' mdl' -> mdl'::acc')
            acc
            mdls')
        []
        mdls 
   
    (**
      [ex_constraint_sat constraints vs s] returns true if and only if
      the stack [s] can be extended with mappings from existential
      variables to values in [vs] such that that the extended stack has
      a mapping for every existential variable mentioned in
      [constraints] and also satisfies [constraints].
    **)
    let exs_satisfiable (eqs, deqs) vs s =
      let unmapped_exs =
        Var.Set.filter
          (fun x -> (Sl_term.is_exist_var (Var.to_term x)) && not (Var.Map.mem x s))
          (Var.Set.of_list
            (List.map Var.of_term
              (List.append
                (Sl_term.Set.to_list (Sl_uf.vars eqs))
                (Sl_term.Set.to_list (Sl_deqs.vars deqs))))) in
      (Var.Set.is_empty unmapped_exs) ||
      Option.is_some
        (valid_extns (eqs, deqs) vs unmapped_exs s List.find_some)

    (**
      [mk_ptos_base defs h] creates a hashtable which stores a hashset
      of model bases for each inductive rule in [defs] that is both
      consistent and contains some number (> 0) of points-to formula
      atoms. These models are the valid interpretations of the entire
      set of points-to atoms in each inductive rule body whose heap is a
      subheap of [h]. The hastable is keyed on a symbolic heap formula.
        i.e. We abstract the points-to set for each rule and compute its
      interpretation only once before starting the fixpoint computation,
      and make it quickly accessible using a hash table.

        Notes:
         1. [all_ptos_itpts] is a map containing all the interpretant
            bases of the singleton subheaps of [h] keyed on size of the
            heap cell being pointed to. This allows easy identification of
            only those subheaps relevant to any given points-to formula
            atom.
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
          let mk_heap_base h' = HeapBase.inj h h' in
          let f loc cell ptos =
            let cell_size = List.length cell in
            let base =
              if Int.Map.mem cell_size ptos then
                Int.Map.find cell_size ptos
              else 
                empty_base () in
            let pto =
              ((Value.mk_loc_val loc)::cell,
                mk_heap_base (Location.Map.singleton loc cell)) in
            InterpretantBase.Hashset.add base pto ;
            Int.Map.add cell_size base ptos in
          Location.Map.fold f h Int.Map.empty in
        let () = debug (fun _ ->
          "Hashmap of Points-to interpretants: " ^
          Int.Map.to_string InterpretantBase.Hashset.to_string all_ptos_itpts) in
        let num_buckets =
          let test_and_incr n rl =
            let body = Sl_indrule.body rl in
            let inc =
              if (Sl_heap.inconsistent body) then 0
              else let (_,_, ptos, _) = Sl_heap.dest body in
                min 1 (Sl_ptos.cardinal ptos) in
            n + inc in
          Sl_defs.rule_fold test_and_incr 0 defs in
        let base = SymHeapHash.create num_buckets in
        let calc_abstractions rl =
          let () = debug (fun _ -> "Calculating points-to base of rule: " ^
            (Sl_indrule.to_string rl)) in
          let (body, _) = Sl_indrule.dest rl in
          let (eqs, deqs, ptos, _) = Sl_heap.dest body in
          let constraints = (eqs, deqs) in
          if (not (Sl_heap.inconsistent body)) &&
             (Sl_ptos.cardinal ptos > 0) then
          begin
            let gen_mdls (t, ts) mdls =
              let pto_models =
                let cell_size = List.length ts in
                if Int.Map.mem cell_size all_ptos_itpts then
                  generate_models (t::ts) constraints
                    (Int.Map.find cell_size all_ptos_itpts)
                else ModelBase.Hashset.create 11 in
              cross_models constraints pto_models mdls in
            let mdls = Sl_ptos.fold gen_mdls ptos (itp_emp ()) in
            SymHeapHash.replace base body (ModelBase.Hashset.to_list mdls)
          end in
        let () = Sl_defs.rule_iter calc_abstractions defs in
        base

    let saturate_ls valset params constraints mdls =
      debug (fun _ -> "Starting universal variable saturation") ;
      let mdls = 
        saturate_univs_ls
          (Var.Set.of_list (List.map Var.of_term params)) constraints valset mdls in
      debug (fun _ -> "Candidate models after universal variable saturation: " ^
        (ModelBase.FList.to_string mdls)) ;
      let mdls = 
        Blist.rev_filter 
          (fun (s,_) -> exs_satisfiable constraints valset s) 
          mdls in
      debug (fun _ -> "Generated models after filtering for existential saturation: " ^
        (ModelBase.FList.to_string mdls)) ;
      mdls 
    
    let add_itpts_of_models_ls params itpts mdls =
      List.iter 
        (fun (s, ls) ->
          let vs = List.map
            (fun x -> let x = Var.of_term x in Var.Map.find x s)
            params in
            InterpretantBase.Hashset.add itpts (vs, ls)
        )
        mdls
        
    (* The function that generates new interpretants for a given rule *)
    let rule_gen ptos_base valset itp itp_acc rl =
      let predsym = Sl_indrule.predsym rl in
      let body = Sl_indrule.body rl in
      let (eqs, deqs, ptos, inds) = Sl_heap.dest body in
      let constraints = (eqs, deqs) in
      let params = Sl_indrule.formals rl in
      let prev_itpts = Sl_predsym.Map.find predsym itp_acc in
      if (Sl_heap.inconsistent body) || (
         (Sl_tpreds.is_empty inds) &&
           let (ancestors, parents) = Sl_predsym.Map.find predsym itp in
           (not (InterpretantBase.Hashset.is_empty ancestors)) ||
           not (InterpretantBase.Hashset.is_empty parents)) then
        let () = debug (fun _ -> "Skipping over rule " ^ (Sl_indrule.to_string rl)) in
        (* stop rule_gen here *)
        ()
      else
      begin
        debug 
          (fun _ -> "Generating new interpretants for rule: " ^ 
          (Sl_indrule.to_string rl)) ;
        let ptos_models =
          if Sl_ptos.is_empty ptos then [(Stack.empty, HeapBase.empty)]
          else SymHeapHash.find ptos_base body in
        debug (fun _ -> "Found the following interpretation for points-tos: " ^
          (ModelBase.FList.to_string ptos_models)) ;
        if Sl_tpreds.is_empty inds then
          add_itpts_of_models_ls
            params 
            prev_itpts
            (saturate_ls valset params constraints ptos_models)
            (* end rule_gen here *)
        else
        begin
          let get_mdls p ms gen_ancestors =
            let p_sym = Sl_tpred.predsym p in
            let p_args = Sl_tpred.args p in
            let (ancestors, parents) = 
              Pair.map InterpretantBase.Hashset.to_list (Sl_predsym.Map.find p_sym itp) in
            let parent_mdls = generate_models_ls p_args constraints parents in
            let prod_from_parents = cross_models_ls constraints ms parent_mdls in
            let ls = [prod_from_parents] in
            if gen_ancestors then
              let ancestor_mdls = generate_models_ls p_args constraints ancestors in
              let prod_from_ancestors = cross_models_ls constraints ms ancestor_mdls in
              (prod_from_ancestors)::ls
            else ls in
          let split p (ms, flag) =
            let mdls = get_mdls p ms true in
            (List.hd mdls, flag) :: List.map (fun m -> (m, true)) (List.tl mdls) in
          let tie p (ms, flag) =
            let mdls = get_mdls p ms flag in
            List.iter
              (fun ms ->
                let candidates = saturate_ls valset params constraints ms in
                debug (fun _ -> "Generated the following candidate models: " ^
                  (ModelBase.FList.to_string candidates)) ;
                add_itpts_of_models_ls
                  params 
                  prev_itpts
                  candidates
              )
              mdls in
          let join = fun _ -> () in
          let seed = (ptos_models, false) in
          Sl_tpreds.weave split tie join inds seed ;
          let () = debug (fun _ -> "Generated the following interpretants: " ^
            (InterpretantBase.Hashset.to_string prev_itpts)) in
          debug (fun _ -> "New interpretation after adding new interpretants: " ^
            (Sl_predsym.Map.to_string InterpretantBase.Hashset.to_string itp_acc))
        end 
      end
      
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
      debug (fun _ -> Value.Set.to_string valset) ;
      debug (fun _ ->
        SymHeapHashPrinter.to_string 
          Sl_heap.to_string ModelBase.FList.to_string ptos_base) ;
      let itp = init_empty defs in
      let new_itp = 
        Sl_predsym.Map.of_list 
          (List.map 
            (fun pd -> 
              let (_,p) = Sl_preddef.dest pd in
              (p, empty_base ())
            ) 
            (Sl_defs.to_list defs)
          ) in
      let rec generator () =
        let () = debug (fun _ -> "Beginning next fixpoint interation") in
        (* Generate the new interpretants for each rule *)
        Sl_defs.rule_iter (rule_gen ptos_base valset itp new_itp) defs ;
        let () = debug (fun _ -> "New interpretants after iteration: " ^
          (Sl_predsym.Map.to_string InterpretantBase.Hashset.to_string new_itp)) in
        
        (* Add the new interpretants to the old ones *)
        Sl_predsym.Map.iter
          (fun p zs ->
            let (xs,ys) = Sl_predsym.Map.find p itp in
            let _ = InterpretantBase.Hashset.left_union xs ys in
            InterpretantBase.Hashset.clear ys ;
            InterpretantBase.Hashset.iter
              (fun z ->
                if not (InterpretantBase.Hashset.mem xs z) then
                  InterpretantBase.Hashset.add ys z)
              zs ;
            InterpretantBase.Hashset.clear zs            
          ) 
          new_itp ;
        
        debug (fun _ -> "result of iteration: " ^
          (Sl_predsym.Map.to_string InterpretantBase.Hashset.to_string new_itp)) ;
          
        (* if all "new" sets of interpretants are empty then stop else recurse *)
        if Sl_predsym.Map.for_all
          (fun _ (_, itpts) -> InterpretantBase.Hashset.is_empty itpts)
          itp
        then
          itp
        else
          generator () in
      generator

    let mk (defs, (vs, h)) =
      let generator = mk_generator (defs, (vs, h)) in
      let base = generator () in
      debug (fun _ -> Sl_predsym.Map.to_string baseSetPair_to_string base) ;
      decorate h base

    let check_model intuitionistic defs (sh, (stk, h)) =
      let f = [sh] in
      let defs = Sl_defs.relevant_defs defs f in
      let () = Sl_defs.check_form_wf defs f in
      let defs = Sl_defs.of_formula defs f in
      let new_def = List.hd (Sl_defs.to_list defs) in
      let new_predsym = Sl_preddef.predsym new_def in
      let vals =
        let rl =
          let rls = Sl_preddef.rules new_def in
          assert (List.length rls == 1) ;
          List.hd rls in
        let formals = Sl_indrule.formals rl in
        List.map
          (fun x -> let x = Var.of_term x in Var.Map.find x stk)
          formals in
      let () = debug (fun _ -> Sl_defs.to_string defs) in
      let () = debug (fun _ -> Stack.to_string stk) in
      let () = debug (fun _ -> Heap.to_string h) in
      let () = debug (fun _ -> Sl_preddef.to_string new_def) in
      let () = debug (fun _ -> Value.FList.to_string vals) in
      let interp = mk (defs, (vals, h)) in
      let heapbase = HeapBase.inj h h in
      if intuitionistic then
        InterpretantBase.Hashset.exists
          (fun (vals',_) -> Value.FList.equal vals vals')
          (Sl_predsym.Map.find new_predsym interp)
      else
        InterpretantBase.Hashset.mem
          (Sl_predsym.Map.find new_predsym interp)
          (vals, heapbase)

  end

module IntSig : ValueSig
  with type HeapLocation.t = NatType.t
  with type ScalarValue.t = NatType.t
    =
  struct
    module HeapLocation =
      struct
        include NatType
        let to_string n = Printf.sprintf "0x%x" n
        let pp fmt n = Format.fprintf fmt "0x%x" n
      end
    module ScalarValue = NatType
    let pp_nil fmt = Format.fprintf fmt "nil"
  end
