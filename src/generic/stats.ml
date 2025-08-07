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

module MC = TimeStats (struct end)

module CC = TimeStats (struct end)

module Gen = TimeStats (struct end)

module Invalidity = TimeStats (struct end)

module Minimization = TimeStats (struct end)


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

module MCCache = CacheStats (struct end)

type t = {
  min_cpu : float ;
  gen_cpu : float ;
  mc_cpu : float ;
  mc_calls : int ;
  mc_rejects : int ;
  mc_cache_cpu : float ;
  mc_cache_queries : int ;
  mc_cache_hits : int ;
  cc_cpu : float ;
  cc_calls : int ;
  cc_rejects : int ;
  invalidity_cpu : float ;
  invalidity_calls : int ;
  invalidity_rejects : int ;
}

let get_stats () = {
  min_cpu = !Minimization.cpu_time ;
  gen_cpu = !Gen.cpu_time ;
  mc_cpu = !MC.cpu_time ;
  mc_calls = !MC.calls ;
  mc_rejects = !MC.rejects ;
  mc_cache_cpu = !MCCache.cpu_time ;
  mc_cache_queries = !MCCache.queries ;
  mc_cache_hits = !MCCache.hits ;
  cc_cpu = !CC.cpu_time ;
  cc_calls = !CC.calls ;
  cc_rejects = !CC.rejects ;
  invalidity_cpu = !Invalidity.cpu_time ;
  invalidity_calls = !Invalidity.calls ;
  invalidity_rejects = !Invalidity.rejects ;
}

let pp_stats fmt (stats : t) =
  Format.fprintf fmt "GENERAL: Minimization took: %f ms.@."
    (1000.0 *. stats.min_cpu) ;
  Format.fprintf fmt "GENERAL: Elapsed process time: %.0f ms.@."
    (1000.0 *. stats.gen_cpu) ;
  Format.fprintf fmt
    "MODCHECK: Absolute time spent model checking: %f ms.@."
    (1000.0 *. stats.mc_cpu) ;
  Format.fprintf fmt
    "MODCHECK: Percentage of process time spent model checking: %.0f%%.@."
    ( if Stdlib.( = ) stats.gen_cpu 0. then 0.
      else 100.0 *. stats.mc_cpu /. stats.gen_cpu ) ;
  Format.fprintf fmt "MODCHECK: Rejected %d out of %d calls.@."
    stats.mc_rejects
    stats.mc_calls ;
  Format.fprintf fmt "MCCACHE: Hits: %d out of %d queries.@."
    stats.mc_cache_hits
    stats.mc_cache_queries ;
  Format.fprintf fmt "MCCACHE: Time spent caching: %.0f ms.@."
    (1000.0 *. stats.mc_cache_cpu) ;
  Format.fprintf fmt "SLSAT: Total time spent: %.0f ms.@."
    (1000.0 *. stats.cc_cpu) ;
  Format.fprintf fmt "SLSAT: Percentage of process time spent: %.0f%%.@."
    ( if Stdlib.( = ) stats.gen_cpu 0. then 0.
      else 100.0 *. stats.cc_cpu /. stats.gen_cpu ) ;
  Format.fprintf fmt
    "SLSAT: %d out of %d predicate definitions were inconsistent.@."
      stats.cc_rejects
      stats.cc_calls ;
  Format.fprintf fmt "INVAL: Total time spent: %.0f ms.@."
    (1000.0 *. stats.invalidity_cpu) ;
  Format.fprintf fmt "INVAL: Percentage of process time spent: %.0f%%.@."
    ( if Stdlib.( = ) stats.gen_cpu 0. then 0.
      else 100.0 *. stats.invalidity_cpu /. stats.gen_cpu ) ;
  Format.fprintf fmt "INVAL: Found as invalid %d out of %d calls.@."
    stats.invalidity_rejects
    stats.invalidity_calls

let gen_print () =
  if !do_statistics then (pp_stats Format.std_formatter (get_stats()))

let reset () =
  Gen.reset () ;
  MC.reset () ;
  CC.reset () ;
  MCCache.reset () ;
  Invalidity.reset ();
  Minimization.reset ()
