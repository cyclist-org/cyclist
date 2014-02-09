external create_aut : int -> unit = "create_aut" ;; 
external destroy_aut: unit -> unit = "destroy_aut" ;;
external create_vertex : int -> unit = "create_vertex" ;;
external tag_vertex : int -> int -> unit = "tag_vertex" ;;
external set_successor : int -> int -> unit = "set_successor" ;;
external set_trace_pair : int -> int -> int -> int -> unit = "set_trace_pair" ;;
external set_progress_pair : int -> int -> int -> int -> unit = "set_progress_pair" ;;
external check_soundness : unit -> bool = "check_soundness" ;;
external set_initial_vertex : int -> unit = "set_initial_vertex" ;; 

create_aut 4 ;;
let t1 = 1 ;; (* create_tag () ;; *)
let t2 = 2 ;; (* create_tag () ;; *)

let v1 = 1 ;;
create_vertex v1 ;;
set_initial_vertex v1 ;;
tag_vertex v1 t1 ;;
tag_vertex v1 t2 ;;

let v2 = 2 ;;
create_vertex v2 ;; 
tag_vertex v2 t2 ;;

let v3 = 3 ;;
create_vertex v3 ;; 
tag_vertex v3 t1 ;;
tag_vertex v3 t2 ;;

let v4 = 4 ;; 
create_vertex v4 ;;
tag_vertex v4 t1 ;;
tag_vertex v4 t2 ;;

let	v5 = 5 ;;
create_vertex v5 ;; 
tag_vertex v5 t2 ;;

set_successor v1 v2 ;;
set_successor v1 v3 ;;
set_successor v3 v5 ;;
set_successor v3 v4 ;;
set_successor v4 v1 ;;


set_trace_pair v1 v2 t2 t2 ;;

set_trace_pair v1 v3 t1 t1 ;;
set_trace_pair v1 v3 t2 t2 ;;

set_trace_pair v3 v4 t1 t1 ;;
set_trace_pair v3 v4 t2 t2 ;;

set_trace_pair v3 v5 t2 t2 ;;

set_trace_pair v4 v1 t1 t1 ;;
set_trace_pair v4 v1 t2 t2 ;;

(* set_progress_pair v1 v3 t1 t1 ;; *) 

print_int (if check_soundness () then 1 else 0) ;;
Printf.printf "\n" ;

destroy_aut () ;;

