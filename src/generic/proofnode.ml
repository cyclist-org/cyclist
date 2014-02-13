open Lib
open Util
open Symbols

let ltx_axiom ax = ltx_paren (ltx_text ax)
let ltx_rule r = ltx_paren (ltx_text r)
  
module Make(Seq: Cycprover.S) =
struct
  type sequent = Seq.t
  
  type proof_subnode =
    | OpenNode
    | AxiomNode
    | InfNode of (int * TagPairs.t * TagPairs.t) list * bool
    | BackNode of int * TagPairs.t
    | AbdNode of int 
  
  type t =
    {
      seq: Seq.t;
      parent: int;
      descr : string;
      node: proof_subnode;
    }
  
  let get_seq n = n.seq
  let get_par n = n.parent
  let dest n = (n.seq, n.parent)
  
  let is_open n = match n.node with
    | OpenNode -> true
    | _ -> false
  (* FIXME remove/redesign "backlinkable" *)
  let is_backlinkable n = match n.node with
    | OpenNode -> true
    | AbdNode _ | AxiomNode | BackNode _ -> false
    | InfNode (_, b) -> true
  let is_backlink n = match n.node with
    | BackNode _ -> true
    | _ -> false
  
  let mk_open seq par_idx =
    {
      seq = seq;
      parent = par_idx;
      node = OpenNode;
      descr = "(Open)"
    }
  
  let mk_axiom seq par_idx ax =
    {
      seq = seq;
      parent = par_idx;
      node = AxiomNode;
      descr = ax
    }
  
  let mk_inf seq par_idx children tvs tps rdesc backt =
    {
      seq = seq;
      parent = par_idx;
      node = InfNode(Blist.zip3 children tvs tps, backt);
      descr = rdesc
    }
  
  let mk_back seq par_idx child tvs rdesc =
    {
      seq = seq;
      parent = par_idx;
      node = BackNode(child, tvs);
      descr = rdesc
    }
  
  let mk_abd seq par_idx child rdesc =
    {
      seq = seq;
      parent = par_idx;
      node = AbdNode(child);
      descr = rdesc      
    }
  
  let to_abstract_node n = match n.node with
    | OpenNode | AxiomNode ->
        Cchecker.mk_abs_node (Seq.tags n.seq) []
    | InfNode(subg, _) ->
        Cchecker.mk_abs_node (Seq.tags n.seq) subg
    | BackNode(child, tv) ->
        Cchecker.mk_abs_node (Seq.tags n.seq) [(child, tv, TagPairs.empty)]
    | AbdNode(child) ->
    (* FIXME this demands tag globality *)
        let tags = Seq.tags n.seq in
        Cchecker.mk_abs_node tags [(child, TagPairs.mk tags, TagPairs.empty)]
  
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
    | InfNode(p, _) ->
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
    | InfNode(p, _) ->
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
  
  let is_closed_at_helper back n cont = match n.node with
    | AxiomNode -> true
    | OpenNode -> false
    | BackNode _ -> not back
    | AbdNode(child) -> cont child
    | InfNode(p, _) -> Blist.for_all (fun (i,_,_) -> cont i) p
  
end
