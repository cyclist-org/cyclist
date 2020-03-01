#use "topfind";;
#directory "_build/src/seplog";;
#directory "_build/src/generic";;
#directory "_build/src/util";;
#directory "_build/src/slsat";;
#directory "_build/src/while";;
#directory "_build/src/procedure";;
#directory "_build/src/temporal_ctl";;
#directory "_build/src/temporal_ltl";;
#directory "_build/src/mparser";;
;;
#install_printer Format.pp_print_string;;
#install_printer Int.Set.pp;;
#install_printer Int.MSet.pp;;
#install_printer Int.FList.pp;;
#install_printer Strng.Set.pp;;
#install_printer Strng.MSet.pp;;
#install_printer Strng.FList.pp;;
#install_printer Term.pp;;
#install_printer Term.Set.pp;;
#install_printer Subst.pp ;;
#install_printer Term.FList.pp ;;
#install_printer Tpair.pp ;;
#install_printer Uf.pp ;;
#install_printer Deqs.pp ;;
#install_printer Pred.pp ;;
#install_printer Tpred.pp ;;
#install_printer Tpreds.pp ;;
#install_printer Pto.pp ;;
#install_printer Ptos.pp ;;
#install_printer Heap.pp;;
#install_printer Form.pp;;
#install_printer Seq.pp;;
#install_printer Defs.pp;;
#install_printer Indrule.pp;;
#install_printer Basepair.pp;;
#install_printer Basepair.Set.pp;;

Format.set_margin (Sys.command "exit $(tput cols)") ;;

(* let defs = Defs.of_string                                                               *)
(*   "dll {                                                                                   *)
(*   fr=nx * bk=pr => dll(fr,bk,pr,nx) |                                                      *)
(*   fr!=nx * bk!=pr * fr->u',pr * dll(u',bk,fr,nx) => dll(fr,bk,pr,nx)                       *)
(*   }";;                                                                                     *)

(* let seq = Seq.of_string                                                                 *)
(*   "x!=w * w!=t * z!=w * w->t,u * dll(x,u,nil,w) * dll(t,y,w,z) |- dll(x,y,nil,z)";;        *)

(* let (lbps, rbps) = Pair.map (Basepair.pairs_of_form defs) seq ;;                        *)

(* let trm_list =                                                                             *)
(*   Term.Set.to_list                                                                      *)
(*     (Term.Set.add Term.nil                                                           *)
(*       (Term.Set.filter Term.is_free_var (Seq.vars seq))) ;;                       *)

(* let (lbps, rbps) = Pair.map (Basepair.pairs_of_form defs) seq ;;                        *)

(* let map_through sigma v =                                                                  *)
(*   Term.Set.map (fun x -> Uf.find x sigma.Heap.eqs) v ;;                       *)

(* let b_move sigma (v,_) (v',pi') =                                                          *)
(*   Heap.subsumed pi' sigma                                                               *)
(*   &&                                                                                       *)
(*   let (v, v') = Pair.map (map_through sigma) (v, v') in                                    *)
(*   Term.Set.subset v' v ;;                                                               *)
     
(* let a_partition ((v, pi) as bp) sigma =                                                    *)
(*   not (Basepair.Set.exists (fun bp' -> b_move sigma bp bp') rbps) ;;                    *)
    
(* let a_move ((v,pi) as bp) =                                                                *)
(*   Blist.exists (fun sigma -> a_partition bp sigma) (Seq.partitions trm_list pi) ;;      *)

(* #trace a_move;;                                                                            *)
(* #trace a_partition;;                                                                       *)
(* #trace b_move;;                                                                            *)
    
(* let res = Basepair.Set.exists a_move lbps;; *)

let terms = Blist.map Term.of_string ["x"; "y"; "z" ];;

let hps = Seq.partitions terms ;;
