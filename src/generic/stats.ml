let do_statistics = ref false

let now () =
  let times = Unix.times () in
  times.Unix.tms_utime +. times.Unix.tms_stime

let time_since t = now () -. t

module TimeStats (E : sig end) = struct
  let calls = ref 0

  let rejects = ref 0

  let start_time = ref 0.0

  let cpu_time = ref 0.0

  let call () =
    incr calls ;
    start_time := now ()

  let end_call () = cpu_time := !cpu_time +. time_since !start_time

  let accept () = end_call ()

  let reject () = incr rejects ; end_call ()

  let reset () =
    calls := 0 ;
    rejects := 0 ;
    start_time := 0.0 ;
    cpu_time := 0.0
end

module MC = TimeStats ()

module CC = TimeStats ()

module Gen = TimeStats ()

module Invalidity = TimeStats ()

module Minimization = TimeStats ()


module CacheStats (E : sig end) = struct
  let queries = ref 0

  let hits = ref 0

  let start_time = ref 0.0

  let cpu_time = ref 0.0

  let call () = start_time := now ()

  let end_call () = cpu_time := !cpu_time +. time_since !start_time

  let hit () = incr hits ; incr queries

  let miss () = incr queries

  let reset () =
    queries := 0 ;
    hits := 0 ;
    start_time := 0.0 ;
    cpu_time := 0.0
end

module MCCache = CacheStats ()

let gen_print () =
  if !do_statistics then (
    Printf.printf "GENERAL: Minimization took: %.0f ms\n"
      (1000.0 *. !Minimization.cpu_time) ;
    Printf.printf "GENERAL: Elapsed process time: %.0f ms\n"
      (1000.0 *. !Gen.cpu_time) ;
    Printf.printf
      "MODCHECK: Absolute time spent model checking: %f ms\n"
      (1000.0 *. !MC.cpu_time) ;
    Printf.printf
      "MODCHECK: Percentage of process time spent model checking: %.0f%%\n"
      ( if Stdlib.( = ) !Gen.cpu_time 0. then 0.
      else 100.0 *. !MC.cpu_time /. !Gen.cpu_time ) ;
    Printf.printf "MODCHECK: Rejected %d out of %d calls.\n" !MC.rejects
      !MC.calls ;
    Printf.printf "MCCACHE: Hits: %d out of %d queries.\n" !MCCache.hits
      !MCCache.queries ;
    Printf.printf "MCCACHE: Time spent caching: %.0f ms \n"
      (1000.0 *. !MCCache.cpu_time) ;
    Printf.printf "SLSAT: Total time spent: %.0f ms\n" (1000.0 *. !CC.cpu_time) ;
    Printf.printf "SLSAT: Percentage of process time spent: %.0f%%\n"
      ( if Stdlib.( = ) !Gen.cpu_time 0. then 0.
      else 100.0 *. !CC.cpu_time /. !Gen.cpu_time ) ;
    Printf.printf
      "SLSAT: %d out of %d predicate definitions were inconsistent.\n"
      !CC.rejects !CC.calls ;
    Printf.printf "INVAL: Total time spent: %.0f ms\n"
      (1000.0 *. !Invalidity.cpu_time) ;
    Printf.printf "INVAL: Percentage of process time spent: %.0f%%\n"
      ( if Stdlib.( = ) !Gen.cpu_time 0. then 0.
      else 100.0 *. !Invalidity.cpu_time /. !Gen.cpu_time ) ;
    Printf.printf "INVAL: Found as invalid %d out of %d calls.\n"
      !Invalidity.rejects !Invalidity.calls )

let reset () =
  Gen.reset () ;
  MC.reset () ;
  CC.reset () ;
  MCCache.reset () ;
  Invalidity.reset ();
  Minimization.reset ()
