open Util
open Lib
open MParser
open Symbols

module List = Blist

module Var :
  sig
    include BasicType with type t = Sl_term.t
    val of_term : Sl_term.t -> t
    exception Not_variable
    val parse : (t, 'a) MParser.t
  end
    =
  struct
    type t = Sl_term.t
    exception Not_variable
    let of_term t = if (Sl_term.is_univ_var t) then t else raise Not_variable
    let equal = Sl_term.equal
    let compare = Sl_term.compare
    let hash = Sl_term.hash
    let pp = Sl_term.pp
    let to_string = Sl_term.to_string
    let parse st = (Sl_term.parse |>> (fun t -> of_term t)) st
  end
  
module ModelChecker =
  struct

    module type S = 
      sig
        
        type value
        type location
        
        module Heap :
          sig
            module Map : OrderedMap with type key = location

            (* include BasicType *)

            type t

            val compare : t -> t -> int
            val equal : t -> t -> bool
            val to_string : t -> string
            val pp : Format.formatter -> t -> unit

            module Parser :
              sig
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (value, 'a) MParser.t 
                    val parse_location : (location, 'a) MParser.t 
                  end) : S
              end
            
          end

        module Stack :
          sig
            module Map : OrderedMap with type key = Var.t
            
            (* include BasicType *)

            type t

            val compare : t -> t -> int
            val equal : t -> t -> bool
            val to_string : t -> string
            val pp : Format.formatter -> t -> unit
            
            module Parser :
              sig
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig val parse_scalar : (value, 'a) MParser.t end) : S
              end
            
          end
          
        (* module type Interpretation =   *)
        (*   sig                          *)
        (*     type t                     *)
            
        (*     val equal : t -> t -> bool *)
        (*     val phi : t -> t           *)
        (*   end                          *)
        
      end
      
    module type ValueSig =
      sig
        module HeapLocation : BasicType
        module ScalarValue : BasicType
      end
  
    module Make (Sig : ValueSig) : S =
      struct
        
        type value =
          | Nil
          | Location of Sig.HeapLocation.t
          | Scalar of Sig.ScalarValue.t

        type location = Sig.HeapLocation.t
        
        module Value : BasicType with type t = value =
          struct
            type t = value
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
            let hash v = 0
            let pp fmt = function
              | Nil -> Format.fprintf fmt "%s" keyw_nil.str
              | Location(l) -> Sig.HeapLocation.pp fmt l
              | Scalar(v) -> Sig.ScalarValue.pp fmt v
            let to_string v = mk_to_string pp v
          end
          
        module Heap = 
          struct 
            module Map = MakeMap(Sig.HeapLocation)
            module ValList = MakeFList(Value)
            
            type t = (ValList.t) Map.t
            
            let compare h h' = Map.compare ValList.compare h h'
            let equal h h' = Map.equal ValList.equal h h'
            let pp fmt h = 
              Format.fprintf fmt "@[[@ ";
              Map.iter 
                (fun k v -> Format.fprintf fmt "%a%s(%a),@ " 
                  Sig.HeapLocation.pp k symb_mapsto.sep 
                  (Blist.pp pp_commasp Value.pp) v)
                h;
              Format.fprintf fmt "]@]"
            let to_string h = mk_to_string pp h
            
            module Parser =
              struct
                
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (value, 'a) MParser.t 
                    val parse_location : (location, 'a) MParser.t 
                  end) : S =
                  struct
                    
                    let parse st = (
                        Tokens.squares ( 
                        Tokens.comma_sep (
                        T.parse_location >>= (fun l ->
                        parse_symb symb_mapsto >>
                        Tokens.parens (Tokens.comma_sep T.parse_scalar) |>> 
                        (fun vs -> (l, vs)))) |>> 
                        (fun cells -> Map.of_list cells))) st
                    let of_string s = handle_reply (MParser.parse_string parse s ()) 
                    
                  end
                
              end
            
          end
          
        module Stack = 
          struct
            module Map = MakeMap(Var)

            type t = value Map.t
            
            let compare s s' = Map.compare Value.compare s s'
            let equal s s' = Map.equal Value.equal s s'
            let pp fmt h = 
              Format.fprintf fmt "@[[@ ";
              Map.iter 
                (fun k v -> Format.fprintf fmt "%a%s%a,@ " 
                  Sl_term.pp k symb_mapsto.sep Value.pp v)
                h;
              Format.fprintf fmt "]@]"
            let to_string s = mk_to_string pp s
            
            module Parser =
              struct
                
                module type S = 
                  sig
                    val parse : (t, 'a) MParser.t
                    val of_string : string -> t
                  end
                module Make (T : sig 
                    val parse_scalar : (value, 'a) MParser.t 
                  end) : S =
                  struct
                    
                    let parse st = (
                        Tokens.squares ( 
                        Tokens.comma_sep (
                        Var.parse >>= (fun x ->
                        parse_symb symb_mapsto >>
                        T.parse_scalar |>> (fun v -> (x, v)))) |>> 
                        (fun ps -> Map.of_list ps))) st
                    let of_string s = handle_reply (MParser.parse_string parse s ()) 
                    
                  end
                
              end
              
          end
        
      end
            
  end

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

let () =
  gc_setup () ;
  Format.set_margin (Sys.command "exit $(tput cols)") ;
  Arg.parse speclist (fun _ -> raise (Arg.Bad "Stray argument found.")) usage ;
  if !str_model="" then die "-M must be specified." ;
  if !str_symheap="" then die "-F must be specified." ;
  let sh = Sl_heap.of_string !str_symheap in
  let all_defs = Sl_defs.of_channel (open_in !defs_path) in
  let defs = relevant_defs all_defs sh in
  print_endline(Sl_defs.to_string defs) ;

