(** A signature for modules encapsulating abstract inference rules *)
module type S = sig
  type t
  (** The abstract type of inference rules *)
  val pp : Format.formatter -> t -> unit
  (** Pretty print the name of the inference rule *)
end