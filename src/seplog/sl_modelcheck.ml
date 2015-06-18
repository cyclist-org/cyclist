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
        let hash s = Hashtbl.hash_param 50 100 s
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
          Var.Map.for_all
            (fun x v -> not (Var.Map.mem x s') || Value.equal v (Var.Map.find x s'))
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
              (Value.equal (Var.Map.find (Var.of_term t) s) (Var.Map.find (Var.of_term t') s)) in
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
          Sl_uf.for_all
            (fun t t' ->
              Sl_term.is_nil t || Sl_term.is_nil t' ||
              ((not (Var.Map.mem (Var.of_term t) s)) && not (Var.Map.mem (Var.of_term t) s')) ||
              ((not (Var.Map.mem (Var.of_term t') s)) && not (Var.Map.mem (Var.of_term t') s')) ||
              ((Var.Map.mem (Var.of_term t) s) && (Var.Map.mem (Var.of_term t) s')) ||
              ((Var.Map.mem (Var.of_term t') s) && (Var.Map.mem (Var.of_term t') s')) ||
              ((Var.Map.mem (Var.of_term t) s) &&
                (Value.equal (Var.Map.find (Var.of_term t) s) (Var.Map.find (Var.of_term t') s'))) ||
              ((Var.Map.mem (Var.of_term t) s') &&
                (Value.equal (Var.Map.find (Var.of_term t) s') (Var.Map.find (Var.of_term t') s))))
            eqs
              &&
          Sl_deqs.for_all
            (fun (t, t') ->
              Sl_term.is_nil t || Sl_term.is_nil t' ||
              ((not (Var.Map.mem (Var.of_term t) s)) && not (Var.Map.mem (Var.of_term t) s')) ||
              ((not (Var.Map.mem (Var.of_term t') s)) && not (Var.Map.mem (Var.of_term t') s')) ||
              ((Var.Map.mem (Var.of_term t) s) && (Var.Map.mem (Var.of_term t) s')) ||
              ((Var.Map.mem (Var.of_term t') s) && (Var.Map.mem (Var.of_term t') s')) ||
              ((Var.Map.mem (Var.of_term t) s) &&
                not (Value.equal (Var.Map.find (Var.of_term t) s) (Var.Map.find (Var.of_term t') s'))) ||
              ((Var.Map.mem (Var.of_term t) s') &&
                not (Value.equal (Var.Map.find (Var.of_term t) s') (Var.Map.find (Var.of_term t') s))))
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

    type model = Stack.t * Heap.t

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
          let hash b = Hashtbl.hash (b.Bitv_string.bits)
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
    module BaseSetPair = PairTypes(InterpretantBase.Set)(InterpretantBase.Set)

    module SymHeapHash = Hashtbl.Make(Sl_heap)
    module SymHeapHashPrinter = HashtablePrinter.Make(SymHeapHash)
    module ModelBase = MakeComplexType(PairTypes(Stack)(HeapBase))

    module T =
      struct
        type t = BaseSetPair.t Sl_predsym.Map.t
        (* Note that we have not implemented a "true" equality        *)
        (* function between interpretations, but rather one optimised *)
        (* for the model checking fixpoint computation: at each       *)
        (* iteration, we only need check that no new interpretants    *)
        (* have been added.                                           *)
        let equal itp itp' =
          Sl_predsym.Map.for_all
            (fun _ (_, itpts) -> InterpretantBase.Set.is_empty itpts)
            itp'
        (* let equal x y =                                   *)
        (*   let binding_eq (p, (xs, xs')) (p', (ys, ys')) = *)
        (*     Sl_predsym.equal p p' &&                      *)
        (*     InterpretantBase.Set.equal xs ys &&           *)
        (*     InterpretantBase.Set.equal xs' ys' in         *)
        (*   List.equal                                      *)
        (*     binding_eq                                    *)
        (*     (Sl_predsym.Map.bindings x)                   *)
        (*     (Sl_predsym.Map.bindings y)                   *)
      end
    include Fixpoint(T)

    let empty_base = InterpretantBase.Set.empty
    let empty_basepair = (empty_base, empty_base)

    (** [itp_emp] is the minimal set of model bases of the formula emp,
          i.e. the singleton set containing the model base consisting of
          the empty stack and the empty heap base.
     *)
    let itp_emp =
      let itp_emp = ModelBase.Hashset.create 1 in
      let () =
        ModelBase.Hashset.add itp_emp (Stack.empty, HeapBase.empty) in
      itp_emp

    let init_empty defs empty_val =
      List.fold_left
        (fun base def -> Sl_predsym.Map.add
          (Sl_preddef.predsym def)
          empty_val
          base)
        Sl_predsym.Map.empty
        (Sl_defs.to_list defs)

    let decorate h itp =
      let f (ancestors, parents) =
        let acc = InterpretantBase.Hashset.create 11 in
        InterpretantBase.Set.iter (InterpretantBase.Hashset.add acc) ancestors ;
        InterpretantBase.Set.iter (InterpretantBase.Hashset.add acc) parents ;
        acc in
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
    let generate_models ts constraints itpts =
      let models =
        ModelBase.Hashset.create (InterpretantBase.Set.cardinal itpts) in
      let acc_model (vs, ls) =
        match (Stack.of_term_bindings (List.combine ts vs)) with
        | None -> ()
        | Some(stack) ->
            if Stack.satisfies constraints stack then
              ModelBase.Hashset.add models (stack, ls) in
      InterpretantBase.Set.iter acc_model itpts ;
      models

    (**
      Given some pure [constraints] Pi and two sets of model bases [ms]
      and [ms'] representing the interpretation of two formulas (Pi : F)
      and (Pi : G) respectively, [cross_models constraints ms ms']
      generates the set of model bases that denotes the intepretation of
      (Pi : F * G).
    **)
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
      let new_mdls = ModelBase.Hashset.create
        ((ModelBase.Hashset.cardinal ms) * (ModelBase.Hashset.cardinal ms')) in
      let merge (s, ls) =
        let merge_acc (s', ls') =
          if (HeapBase.disjoint ls ls') &&
             (Stack.consistent s s') &&
             (Stack.cross_satisfies constraints s s') then
            let new_mdl =
              let new_stack = Stack.merge s s' in
              let new_heap_spt = HeapBase.union ls ls' in
              (new_stack, new_heap_spt) in
            ModelBase.Hashset.add new_mdls new_mdl in
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
    let valid_extns (eqs, deqs) vs xs s =
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
      fun f -> f test_extn valuations

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
    let saturate_univs params (eqs, deqs) vs mdls =
      let new_mdls =
        ModelBase.Hashset.create (ModelBase.Hashset.cardinal mdls) in
      let acc_saturated_models (s, ls) =
        let unmapped_univs = Var.Set.filter
          (fun x -> (Sl_term.is_univ_var (Var.to_term x)) && not (Var.Map.mem x s))
          (Var.Set.union params
            (Var.Set.of_list
              (List.map Var.of_term
                (List.rev_append
                  (Sl_term.Set.to_list (Sl_uf.vars eqs))
                  (Sl_term.Set.to_list (Sl_deqs.vars deqs)))))) in
        if Var.Set.is_empty unmapped_univs then
          ModelBase.Hashset.add new_mdls (s, ls)
        else
          let good_stacks = Option.list_get
            (List.map |> (valid_extns (eqs, deqs) vs unmapped_univs s)) in
          List.iter
            (fun stk -> ModelBase.Hashset.add new_mdls (stk, ls))
            good_stacks in
      let () = ModelBase.Hashset.iter acc_saturated_models mdls in
      new_mdls

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
            (List.rev_append
              (Sl_term.Set.to_list (Sl_uf.vars eqs))
              (Sl_term.Set.to_list (Sl_deqs.vars deqs))))) in
      (Var.Set.is_empty unmapped_exs) ||
      Option.is_some
        (List.find_some |> (valid_extns (eqs, deqs) vs unmapped_exs s))

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
          let mk_heap_base h' =
            let inj = HeapBase.inj h in
            inj h' in
          let f = fun loc cell ptos ->
            let cell_size = List.length cell in
            let base =
              if Int.Map.mem cell_size ptos then
                Int.Map.find cell_size ptos
              else empty_base in
            let pto =
              ((Value.mk_loc_val loc)::cell,
                mk_heap_base (Location.Map.singleton loc cell)) in
            let base = InterpretantBase.Set.add pto base in
            Int.Map.add cell_size base ptos in
          Location.Map.fold f h Int.Map.empty in
        let () = debug (fun _ -> Int.Map.to_string InterpretantBase.Set.to_string all_ptos_itpts) in
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
          let (body, _) = Sl_indrule.dest rl in
          let (eqs, deqs, ptos, _) = Sl_heap.dest body in
          let constraints = (eqs, deqs) in
          if (not (Sl_heap.inconsistent body)) &&
             (Sl_ptos.cardinal ptos > 0) then
          begin
            let mdls =
              let gen_mdls (t, ts) mdls =
                let pto_models =
                  let cell_size = List.length ts in
                  if Int.Map.mem cell_size all_ptos_itpts then
                    generate_models (t::ts) constraints
                      (Int.Map.find cell_size all_ptos_itpts)
                  else ModelBase.Hashset.create 0 in
                cross_models constraints pto_models mdls in
              Sl_ptos.fold gen_mdls ptos itp_emp in
            SymHeapHash.add base body mdls
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
      let () = debug (fun _ -> SymHeapHashPrinter.to_string Sl_heap.to_string ModelBase.Hashset.to_string ptos_base) in
      let generator itp =
        let () = debug (fun _ -> "Beginning next fixpoint interation") in
        (* The function that generates new interpretants for a given rule *)
        let rule_gen itp_acc rl =
          let predsym = Sl_indrule.predsym rl in
          let body = Sl_indrule.body rl in
          let (eqs, deqs, ptos, inds) = Sl_heap.dest body in
          let constraints = (eqs, deqs) in
          let params = Sl_indrule.formals rl in
          let new_itpts =
            if (Sl_heap.inconsistent body) || (
               (Sl_tpreds.is_empty inds) &&
                 let (ancestors, parents) = Sl_predsym.Map.find predsym itp in
                 (not (InterpretantBase.Set.is_empty ancestors)) ||
                 not (InterpretantBase.Set.is_empty parents)) then
              let () = debug (fun _ -> "Skipping over rule " ^ (Sl_indrule.to_string rl)) in
              InterpretantBase.Set.empty
            else
              let () = debug (fun _ -> "Generating new interpretants for rule: " ^ (Sl_indrule.to_string rl)) in
              let ptos_models =
                if Sl_ptos.is_empty ptos then itp_emp
                else SymHeapHash.find ptos_base body in
              let () = debug (fun _ -> "Found the following interpretation for points-tos: " ^
                (ModelBase.Hashset.to_string ptos_models)) in
              let saturate mdls =
                let () = debug (fun _ -> "Starting universal variable saturation") in
                let mdls = saturate_univs
                  (Var.Set.of_list (List.map Var.of_term params)) constraints valset mdls in
                let () = debug (fun _ -> "Candidate models after universal variable saturation: " ^
                  (ModelBase.Hashset.to_string mdls)) in
                let () = debug (fun _ -> "Starting existential variable saturation") in
                let mdls =
                  let filtered_mdls =
                    ModelBase.Hashset.create
                      (ModelBase.Hashset.cardinal mdls) in
                  let () = ModelBase.Hashset.iter
                    (fun ((s, _) as mdl) ->
                      if (exs_satisfiable constraints valset s) then
                        ModelBase.Hashset.add filtered_mdls mdl)
                    mdls in
                  filtered_mdls in
                let () = debug (fun _ -> "Generated models after filtering for existential saturation: " ^
                  (ModelBase.Hashset.to_string mdls)) in
                mdls in
              let full_models =
                if Sl_tpreds.is_empty inds then
                  saturate ptos_models
                else
                let get_mdls p ms gen_ancestors =
                  let p_sym = Sl_tpred.predsym p in
                  let p_args = Sl_tpred.args p in
                  let (ancestors, parents) = Sl_predsym.Map.find p_sym itp in
                  let parent_mdls = generate_models p_args constraints parents in
                  let prod_from_parents = cross_models constraints ms parent_mdls in
                  let ls = [prod_from_parents] in
                  if gen_ancestors then
                    let ancestor_mdls = generate_models p_args constraints ancestors in
                    let prod_from_ancestors = cross_models constraints ms ancestor_mdls in
                    (prod_from_ancestors)::ls
                  else ls in
                let split p (ms, flag) =
                  let mdls = get_mdls p ms true in
                  (List.hd mdls, flag) ::
                    List.map (fun m -> (m, true)) (List.tl mdls) in
                (* Hashset.create: we can tweak the initial size of the hashset for performance *)
                (* TODO: Change this magic number! *)
                let acc = ModelBase.Hashset.create 10000 in
                let tie p (ms, flag) =
                  let mdls = (get_mdls p ms flag) in
                  List.iter
                    (fun ms ->
                      let candidates = saturate ms in
                      let () = debug (fun _ -> "Generated the following candidate models: " ^
                        (ModelBase.Hashset.to_string candidates)) in
                      ModelBase.Hashset.iter (ModelBase.Hashset.add acc) candidates)
                    mdls in
                let join = fun _ -> () in
                let seed = (ptos_models, false) in
                let () = Sl_tpreds.weave split tie join inds seed in
                acc in
              let itpts = ModelBase.Hashset.fold
                (fun (s, ls) itpts ->
                  let vs = List.map
                    (fun x -> let x = Var.of_term x in Var.Map.find x s)
                    params in
                  InterpretantBase.Set.add (vs, ls) itpts)
                full_models
                empty_base in
              let () = debug (fun _ -> "Generated the following interpretants: " ^
                (InterpretantBase.Set.to_string itpts)) in
              itpts in
          let itp_acc =
            if not (Sl_predsym.Map.mem predsym itp_acc) then
              Sl_predsym.Map.add predsym new_itpts itp_acc
            else
              let prev_itpts = Sl_predsym.Map.find predsym itp_acc in
              Sl_predsym.Map.add predsym
                (InterpretantBase.Set.union prev_itpts new_itpts) itp_acc in
          let () = debug (fun _ -> "New interpretation after adding new interpretants: " ^
            (Sl_predsym.Map.to_string InterpretantBase.Set.to_string itp_acc)) in
          itp_acc in
        (* Generate the new interpretants for each rule *)
        let new_itp = Sl_defs.rule_fold rule_gen Sl_predsym.Map.empty defs in
        let () = debug (fun _ -> "New interpretants after iteration: " ^
          (Sl_predsym.Map.to_string InterpretantBase.Set.to_string new_itp)) in
        (* Add the new interpretants to the old ones *)
        let combined_mapping = List.map2
            (fun (p, (xs, ys)) (p', zs) ->
              assert (Sl_predsym.equal p p') ;
              let ancestors = InterpretantBase.Set.union xs ys in
              let parent = InterpretantBase.Set.diff zs ancestors in
              (p, (ancestors, parent))
              )
            (Sl_predsym.Map.bindings itp)
            (Sl_predsym.Map.bindings new_itp) in
          let new_itp = Sl_predsym.Map.of_list combined_mapping in
          let () = debug (fun _ -> "result of iteration: " ^
            (Sl_predsym.Map.to_string BaseSetPair.to_string new_itp)) in
          new_itp in
      generator

    let mk (defs, (vs, h)) =
      let generator = mk_generator (defs, (vs, h)) in
      let start_itp = init_empty defs empty_basepair in
      let base = fixpoint generator start_itp in
      let () = debug (fun _ -> Sl_predsym.Map.to_string BaseSetPair.to_string base) in
      decorate h base

    let check_model defs (sh, (stk, h)) =
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
      let () = debug (fun _ -> Sl_predsym.Map.to_string InterpretantBase.Hashset.to_string interp) in
      let () = debug (fun _ -> "Looking for #" ^ (string_of_int ((InterpretantBase.hash (vals, heapbase)) land max_int)) ^ ":" ^ (InterpretantBase.to_string (vals, heapbase))) in
      let () = debug (fun _ -> "in " ^ (InterpretantBase.Hashset.to_string (Sl_predsym.Map.find new_predsym interp))) in
      let () = debug (fun _ -> string_of_bool (InterpretantBase.Hashset.mem (Sl_predsym.Map.find new_predsym interp) (vals, heapbase))) in
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

module IntSigModelChecker = Make(IntSig)
open IntSigModelChecker

module IntSigParser =
  struct
    let parse_location st =
      (Tokens.hexadecimal |>> (fun v ->
        if v == 0 then (failwith "0x0 is not a location!") else v)) st
    let parse_scalar st =
      ( attempt (Tokens.hexadecimal |>> (fun v ->
        if v == 0 then Value.zero else (Value.mk_loc_val v)))
        <|>
        (Tokens.integer |>> (fun v -> Value.mk_scalar_val v))
      ) st
  end

module StackParser = Stack.MakeParser(IntSigParser)
module HeapParser = Heap.MakeParser(IntSigParser)

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
  (* TODO: Need to check that all predicate instances in sh match the arity in defs *)
  let defs = Sl_defs.of_channel (open_in !defs_path) in
  let ((s, h) as model) = model_of_string model_parser !str_model in
  let () = print_endline("Heap size: " ^ (Int.to_string (Heap.size h))) in
  begin
    Stats.reset () ;
    Stats.Gen.call () ;
    let call () = check_model defs (sh, model) in
    let res = call () in
    Stats.Gen.end_call () ;
    if !Stats.do_statistics then Stats.gen_print ();
    if res then
      print_endline("Model verified")
    else
      print_endline("Not a satisfying model!")
  end

