#use "topfind";;
#directory "_build/src/seplog";;
#directory "_build/src/generic";;
#directory "_build/src/util";;
#directory "_build/src/goto";;
#directory "_build/src/slsat";;
#directory "_build/src/while";;
#directory "_build/src/mparser";;
open Util;;
#install_printer Format.pp_print_string;;
#install_printer Int.Set.pp;;
#install_printer Int.MSet.pp;;
#install_printer Int.FList.pp;;
#install_printer Int.Pairing.Set.pp;;
#install_printer Int.Pairing.MSet.pp;;
#install_printer Int.Pairing.FList.pp;;
#install_printer Strng.Set.pp;;
#install_printer Strng.MSet.pp;;
#install_printer Strng.FList.pp;;
#install_printer Strng.Pairing.Set.pp;;
#install_printer Strng.Pairing.MSet.pp;;
#install_printer Strng.Pairing.FList.pp;;
#install_printer Sl_term.pp;;
#install_printer Sl_term.Set.pp;;
#install_printer Sl_term.pp_subst ;;
#install_printer Sl_term.FList.pp ;;
#install_printer Sl_tpair.pp ;;
#install_printer Sl_uf.pp ;;
#install_printer Sl_deqs.pp ;;
#install_printer Sl_pred.pp ;;
#install_printer Sl_tpred.pp ;;
#install_printer Sl_tpreds.pp ;;
#install_printer Sl_pto.pp ;;
#install_printer Sl_ptos.pp ;;
#install_printer Sl_heap.pp;;
#install_printer Sl_form.pp;;
#install_printer Sl_seq.pp;;
#install_printer Sl_defs.pp;;

open Lib;;

let seq = Sl_seq.of_string "nil!=x * nil!=y * x!=y * y->z * ls^2(z, nil) * ls^3(w, y) |- x!=z * ls^4(z, nil) * ls^5(w, z)" ;;

let seq' = Sl_seq.of_string "nil!=y * y->z * ls^1(x, y) * ls^2(z, nil) |- ls^3(x, z) * ls^4(z, nil)";;

let theta = Sl_term.singleton_subst (Sl_term.of_string "x") (Sl_term.of_string "w");;

