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
#install_printer Sl_indrule.pp;;
#install_printer Sl_basepair.pp;;
#install_printer Sl_basepair.Set.pp;;

Format.set_margin (Sys.command "exit $(tput cols)") ;;

let defs = Sl_defs.of_channel (open_in "examples/sl.defs");;
let s = Sl_seq.of_string "DLL^1(x, y, z, w) * DLL^2(a, x, w, b) |- DLL^3(a, y, z, b)";;


#trace Sl_basepair.pairs_of_form;;
Sl_seq.invalid defs s ;;

       
