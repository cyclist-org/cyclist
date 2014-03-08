let do_statistics = ref false
    
let now () = (Unix.times ()).Unix.tms_utime
let time_since t = (now ()) -. t

module TimeStats(E : sig end) = 
	struct 
    let calls = ref 0
    let rejects = ref 0
    let start_time = ref 0.0
    let cpu_time = ref 0.0
	
    let call () = incr calls ; start_time := now ()
		let end_call () = cpu_time := !cpu_time +. (time_since !start_time)
		 	
    let accept () = end_call ()
    let reject () = incr rejects ; end_call ()
		
		let reset () = 
			calls := 0;
			rejects := 0;
			start_time := 0.0;
			cpu_time := 0.0
		
	end

module MC = TimeStats(struct end)
module CC = TimeStats(struct end)
module Gen = TimeStats(struct end)

module MCCache = 
  struct 
    let queries = ref 0
    let hits = ref 0
    let start_time = ref 0.0
    let cpu_time = ref 0.0
    
    let call () = start_time := now ()
    let end_call () = cpu_time := !cpu_time +. (time_since !start_time)
            
    let hit () = incr hits ; incr queries
    let miss () = incr queries
          
    let reset () = 
      queries := 0;
      hits := 0;
      start_time := 0.0;
      cpu_time := 0.0
        
  end


let gen_print () =
  if !do_statistics then 
		begin
      Printf.printf "Elapsed CPU time: %.0f ms\n" (1000.0 *. !Gen.cpu_time) ;
      Printf.printf "Percentage of CPU time spent model checking: %.0f%%\n"
        (if !Gen.cpu_time = 0. then 0. else (100.0 *. !MC.cpu_time /. !Gen.cpu_time)) ;
      print_endline ("Model checker calls: Rejected " ^ (string_of_int !MC.rejects) ^
    		" out of " ^ (string_of_int !MC.calls) ^ " calls.") ;
		  print_endline ("Model checker cache: Hits: " ^ (string_of_int (!MCCache.hits)) ^
			  " out of " ^ (string_of_int !MCCache.queries) ^ " queries.") ;
		  Printf.printf "Model checker cache: Time spent caching: %.0f ms \n" (1000.0 *. !MCCache.cpu_time) ;
      Printf.printf "Percentage of CPU time spent consistency checking: %.0f%%\n"
        (if !Gen.cpu_time = 0. then 0. else (100.0 *. !CC.cpu_time /. !Gen.cpu_time)) ;
      print_endline 
  		  ("Consistency checker calls: " ^ (string_of_int !CC.rejects) ^ 
  			" out of " ^ (string_of_int !CC.calls) ^ " predicate definitions were inconsistent.")
		end
  
let reset () =
  Gen.reset ();
	MC.reset ();
	CC.reset (); 
	MCCache.reset ()
