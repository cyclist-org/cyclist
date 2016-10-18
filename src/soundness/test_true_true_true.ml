external create_fair_aut : int -> unit = "create_fair_aut" ;; 
external destroy_fair_aut: unit -> unit = "destroy_fair_aut" ;;
external create_fair_vertex : int -> int -> unit = "create_fair_vertex" ;;
external tag_fair_vertex : int -> int -> unit = "tag_fair_vertex" ;;
external set_fair_successor : int -> int -> unit = "set_fair_successor" ;;
external set_fair_trace_pair : int -> int -> int -> int -> unit = "set_fair_trace_pair" ;;
external set_fair_progress_pair : int -> int -> int -> int -> unit = "set_fair_progress_pair" ;;
external check_fair_soundness : unit -> bool = "check_fair_soundness" ;;
external set_initial_fair_vertex : int -> unit = "set_initial_fair_vertex" ;; 

create_fair_aut 4 ;;
let t1 = 1 ;; (* create_tag () ;; *)
let t2 = 2 ;; (* create_tag () ;; *)

let c1 = 10 ;; (* create_command_label *)
let c2 = 20 ;; (* create_command_label *)
let c3 = 30 ;; (* create_command_label *)
let c4 = 40 ;; (* create_command_label *)
let c5 = 50 ;; (* create_command_label *)
  
let v1 = 1 ;;
create_fair_vertex v1 c1;;
set_initial_fair_vertex v1 ;;
tag_fair_vertex v1 t1 ;;
tag_fair_vertex v1 t2 ;;

let v2 = 2 ;;
create_fair_vertex v2 c2 ;; 
tag_fair_vertex v2 t2 ;;

let v3 = 3 ;;
  create_fair_vertex v3 c3;; 
tag_fair_vertex v3 t1 ;;
tag_fair_vertex v3 t2 ;;

let v4 = 4 ;; 
create_fair_vertex v4 c4;;
tag_fair_vertex v4 t1 ;;
tag_fair_vertex v4 t2 ;;

let v5 = 5 ;;
create_fair_vertex v5 c5 ;; 
tag_fair_vertex v5 t2 ;;

set_fair_successor v1 v2 ;;
set_fair_successor v1 v3 ;;
set_fair_successor v3 v5 ;;
set_fair_successor v3 v4 ;;
set_fair_successor v4 v1 ;;


set_fair_trace_pair v1 v2 t2 t2 ;;

set_fair_trace_pair v1 v3 t1 t1 ;;
set_fair_trace_pair v1 v3 t2 t2 ;;

set_fair_trace_pair v3 v4 t1 t1 ;;
set_fair_trace_pair v3 v4 t2 t2 ;;

set_fair_trace_pair v3 v5 t2 t2 ;;

set_fair_trace_pair v4 v1 t1 t1 ;;
set_fair_trace_pair v4 v1 t2 t2 ;;

set_fair_progress_pair v1 v3 t1 t1 ;;

print_int (if check_fair_soundness () then 1 else 0) ;;
Printf.printf "\n" ;

destroy_fair_aut () ;;

