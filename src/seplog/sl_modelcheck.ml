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
    let of_term t = if (Sl_term.is_univ_var t) then t else raise Not_variable
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
          
        module type Model = 
          sig
            include BasicType with type t = Value.FList.t * Heap.t
            module Set : OrderedContainer with type elt = t
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
            type t
            
            val empty : t
            val equal : t -> t -> bool
            val mk_generator : Sl_defs.t * Interpretant.t -> t -> t
            val dest : t -> Interpretant.Set.t Sl_predsym.Map.t
          end
            =
          struct
              
            (* Dummy code *)
            type t = Dummy
            let empty = Dummy
            let equal x y = true
            let mk_generator (defs, (vs, h)) x = x
            let dest x = Sl_predsym.Map.empty
          end
          
        include Fixpoint(Interpretation)
          
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
          let interp =
            let fp = fixpoint
              (Interpretation.mk_generator (defs, (vals, h))) 
              Interpretation.empty in
            Interpretation.dest fp in
          
          print_endline(Sl_defs.to_string defs) ;
          print_endline(Stack.to_string stk) ;
          print_endline(Heap.to_string h) ;
          print_endline(Sl_preddef.to_string new_def) ;
          print_endline(Value.FList.to_string vals) ;
          
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
  let res = check_model (sh, model) in
  ();

