open Lib
open Util
open Symbols

let ltx_axiom ax = ltx_paren (ltx_text ax)
let ltx_rule r = ltx_paren (ltx_text r)
  
module Make(Seq: Sigs.SEQUENT) =
struct
  module Seq = Seq
  
  type proof_subnode =
    | OpenNode
    | AxiomNode
    | InfNode of (int * TagPairs.t * TagPairs.t) list
    | BackNode of int * TagPairs.t
    | AbdNode of int 
  
  type t =
    {
      seq: Seq.t;
      descr : string;
      node: proof_subnode;
    }
  
  let get_seq n = n.seq
  let get_succs n = match n.node with
    | AxiomNode | OpenNode -> []
    | BackNode (s, _) | AbdNode(s) -> [s]
    | InfNode(ss) -> let (ss',_,_) = Blist.unzip3 ss in ss'
  
  let dest n = (n.seq, n.descr)
  let dest_abd n = match n.node with
    | AbdNode(child) -> (n.seq, n.descr, child)
    | _ -> invalid_arg "dest_abd"
  let dest_backlink n = match n.node with
    | BackNode(child, vtts) -> (n.seq, n.descr, child, vtts)
    | _ -> invalid_arg "dest_backlink"
  let dest_inf n = match n.node with
    | InfNode(subgs) -> (n.seq, n.descr, subgs)
    | _ -> invalid_arg "dest_inf"
  
  
  let is_open n = match n.node with
    | OpenNode -> true
    | _ -> false
  let is_backlink n = match n.node with
    | BackNode _ -> true
    | _ -> false
  let is_axiom n = match n.node with
    | AxiomNode -> true
    | _ -> false
  let is_abd n = match n.node with
    | AbdNode _ -> true
    | _ -> false
  let is_inf n = match n.node with
    | InfNode _ -> true
    | _ -> false


  let mk seq node descr =
    {
      seq = seq;
      node = node;
      descr = descr
    }
    
  let mk_open seq = mk seq OpenNode "(Open)"
  let mk_axiom seq descr = mk seq AxiomNode descr
  let mk_abd seq descr child = mk seq (AbdNode(child)) descr
  let mk_inf seq descr subgoals = mk seq (InfNode(subgoals)) descr
  let mk_backlink seq descr child vtts = mk seq (BackNode(child, vtts)) descr
  
  let to_abstract_node n = match n.node with
    | OpenNode | AxiomNode ->
        Soundcheck.mk_abs_node (Seq.tags n.seq) []
    | InfNode(subg) ->
        Soundcheck.mk_abs_node (Seq.tags n.seq) subg
    | BackNode(child, tv) ->
        Soundcheck.mk_abs_node (Seq.tags n.seq) [(child, tv, TagPairs.empty)]
    | AbdNode(child) ->
    (* FIXME this demands tag globality *)
        let tags = Seq.tags n.seq in
        Soundcheck.mk_abs_node tags [(child, TagPairs.mk tags, TagPairs.empty)]
  
  let pp fmt id n cont = match n.node with
    | OpenNode ->
        Format.fprintf fmt "@[%i: %a (Open)@]"
          id Seq.pp n.seq
    | AxiomNode ->
        Format.fprintf fmt "@[%i: %a (%s)@]"
          id Seq.pp n.seq n.descr
    | BackNode(i, _) ->
        Format.fprintf fmt "@[%i: %a (%s) [%i]@]"
          id Seq.pp n.seq n.descr i
    | InfNode(p) ->
        Format.fprintf fmt "@[<v 2>%i: %a (%s) [%a]@,%a@]"
          id
          Seq.pp n.seq
          n.descr
          (Blist.pp pp_comma (fun fmt (i,_,_) -> Format.pp_print_int fmt i)) p
          (Blist.pp Format.pp_print_newline (fun fmt' (i,_,_) -> cont fmt i)) p
    | AbdNode(child) ->
        Format.fprintf fmt "@[<v 2>%i: %a (%s) [%i]@,%a@]"
          id
          Seq.pp n.seq
          n.descr
          child
          cont child
  
  let justify = Latex.text "\n\\justifies\n\\thickness=0.1em\n"
  let using = Latex.text "\\using"
  let prooftree first seq m =
    let comment = Latex.text ("% " ^ (Seq.to_string seq) ^ "\n") in
    if first then
      Latex.environment
        "prooftree" (Latex.M, (Latex.concat [comment; m])) Latex.M
    else
      Latex.concat [ Latex.text "\\[ "; comment; m; Latex.text "\n\\]\n" ]
  let prefix id s =
    Latex.concat
      [ Latex.text ((string_of_int id) ^ " : "); Seq.to_melt s]
  let justifies id s = Latex.concat [ justify; prefix id s; ltx_newl ]
  
  let to_melt first id n cont = match n.node with
    | OpenNode ->
        ltx_mk_math
          (Latex.concat [ prefix id n.seq; ltx_text "(Open)"; ltx_newl ])
    | AxiomNode ->
        prooftree first n.seq
          (Latex.concat [ ltx_axiom n.descr; justifies id n.seq ])
    | InfNode(p) ->
        prooftree first n.seq
          (Latex.concat
              ((Blist.map (fun (i,_,_) -> cont false i) p) @
                [ justifies id n.seq; using; ltx_rule n.descr; ltx_newl ]))
    | BackNode(i, _) ->
        prooftree first n.seq
          (Latex.concat
              [ Latex.text ("\\to " ^ (string_of_int i));
              ltx_rule n.descr; justifies id n.seq ])
    | AbdNode(child) ->
        prooftree first n.seq
          (Latex.concat
              [ cont false child ;
              justifies id n.seq; using; ltx_rule n.descr; ltx_newl ])
  
end
