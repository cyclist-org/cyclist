val do_statistics : bool ref
val now : unit -> float
val time_since : float -> float

module TimeStats (E : sig end) : sig
  val calls : int ref
  val rejects : int ref
  val start_time : float ref
  val cpu_time : float ref
  val call : unit -> unit
  val end_call : unit -> unit
  val accept : unit -> unit
  val reject : unit -> unit
  val reset : unit -> unit
end

module MC : module type of TimeStats (struct end)
module CC : module type of TimeStats (struct end)
module Gen : module type of TimeStats (struct end)
module Invalidity : module type of TimeStats (struct end)
module Minimization : module type of TimeStats (struct end)

module CacheStats (E : sig end) : sig
  val queries : int ref
  val hits : int ref
  val start_time : float ref
  val cpu_time : float ref
  val call : unit -> unit
  val end_call : unit -> unit
  val hit : unit -> unit
  val miss : unit -> unit
  val reset : unit -> unit
end

module MCCache : module type of CacheStats (struct end)

val gen_print : unit -> unit
val reset : unit -> unit
