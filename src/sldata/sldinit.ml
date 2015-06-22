#use "topfind";;
#directory "_build/src/seplog";;
#directory "_build/src/generic";;
#directory "_build/src/util";;
#directory "_build/src/goto";;
#directory "_build/src/slsat";;
#directory "_build/src/while";;
#directory "_buld/src/extended_while";;
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

(* let defs = Sl_defs.of_string                                                               *)
(*   "dll {                                                                                   *)
(*   fr=nx * bk=pr => dll(fr,bk,pr,nx) |                                                      *)
(*   fr!=nx * bk!=pr * fr->u',pr * dll(u',bk,fr,nx) => dll(fr,bk,pr,nx)                       *)
(*   }";;                                                                                     *)

(* let seq = Sl_seq.of_string                                                                 *)
(*   "x!=w * w!=t * z!=w * w->t,u * dll(x,u,nil,w) * dll(t,y,w,z) |- dll(x,y,nil,z)";;        *)

(* let (lbps, rbps) = Pair.map (Sl_basepair.pairs_of_form defs) seq ;;                        *)

(* let trm_list =                                                                             *)
(*   Sl_term.Set.to_list                                                                      *)
(*     (Sl_term.Set.add Sl_term.nil                                                           *)
(*       (Sl_term.Set.filter Sl_term.is_univ_var (Sl_seq.vars seq))) ;;                       *)

(* let (lbps, rbps) = Pair.map (Sl_basepair.pairs_of_form defs) seq ;;                        *)

(* let map_through sigma v =                                                                  *)
(*   Sl_term.Set.endomap (fun x -> Sl_uf.find x sigma.Sl_heap.eqs) v ;;                       *)

(* let b_move sigma (v,_) (v',pi') =                                                          *)
(*   Sl_heap.subsumed pi' sigma                                                               *)
(*   &&                                                                                       *)
(*   let (v, v') = Pair.map (map_through sigma) (v, v') in                                    *)
(*   Sl_term.Set.subset v' v ;;                                                               *)
     
(* let a_partition ((v, pi) as bp) sigma =                                                    *)
(*   not (Sl_basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps) ;;                    *)
    
(* let a_move ((v,pi) as bp) =                                                                *)
(*   Blist.exists (fun sigma -> a_partition bp sigma) (Sl_seq.partitions trm_list pi) ;;      *)

(* #trace a_move;;                                                                            *)
(* #trace a_partition;;                                                                       *)
(* #trace b_move;;                                                                            *)
    
(* let res = Sl_basepair.Set.exists a_move lbps;; *)

let terms = Blist.map Sl_term.of_string ["x"; "y"; "z" ];;

let hps = Sl_seq.partitions terms ;;
