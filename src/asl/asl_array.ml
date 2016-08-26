open Lib
open Util
open Symbols
open MParser

include PairTypes(Asl_term)(Asl_term)

let subst theta (t1, t2) =
  (Asl_term.subst theta t1, Asl_term.subst theta t2)

let unify ?(sub_check=Asl_subst.trivial_check)
    ?(cont=Asl_unifier.trivial_continuation)
    ?(init_state=Asl_unifier.empty_state) a a' =
  Asl_tpair.unify ~order:true ~sub_check ~cont ~init_state a a'

let to_string (t1,t2) =
  symb_array.str ^ symb_lp.str ^ (Asl_term.to_string t1)  ^ symb_comma.sep ^
  (Asl_term.to_string t2) ^ symb_rp.str
  
let to_melt (t1,t2) =
  Latex.concat
    [ symb_array.melt; symb_lp.melt; Asl_term.to_melt t1; symb_comma.melt;
    Asl_term.to_melt t2; symb_rp.melt ]

let terms (t1, t2) = Asl_term.Set.of_list [t1; t2]

let vars arr = Asl_term.filter_vars (terms arr) 

let norm eqs (t1, t2) = 
  (Asl_uf.find t1 eqs, Asl_uf.find t2 eqs)

let parse st =
  let parse_terms st =
    (Asl_term.parse >>= (fun x-> parse_symb symb_comma >> Asl_term.parse << spaces |>> (fun y -> (x, y)))) st
  in (parse_symb symb_array >>
    MParser_PCRE.Tokens.parens parse_terms << spaces <?> "arr") st
