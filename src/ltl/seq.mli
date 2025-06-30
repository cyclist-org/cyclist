open Generic

include Sequent.S

val singleton : Form.t -> t
val of_list : Form.t list -> t
val of_string : string -> t

val to_list : t -> Form.t list

val get_tag : Form.t -> t -> Tags.Elt.t option

val is_axiomatic : t -> bool
val is_empty : t -> bool

val add : Form.t -> t -> t
val remove : Form.t -> t -> t

val add_all : Form.t list -> t -> t

val exists : (Form.t -> bool) -> t -> bool
val find_suchthat_opt : (Form.t -> bool) -> t -> Form.t option

val subset : t -> t -> bool

val inter : t -> t -> t
val diff : t -> t -> t

val fold : (Form.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
val fold_with_tags : (Tags.Elt.t * Form.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc