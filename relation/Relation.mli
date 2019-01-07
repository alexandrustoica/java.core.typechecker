open Type
open Class
open Program

type relation = Relation of system_type * system_type

(** Returns the first element in our relation pair. *)
val head: relation -> system_type

(** Returns the second element in our relation pair. *)
val tail: relation -> system_type

val tails_for: system_type -> relation list -> system_type list
val compare: relation -> relation -> bool
val string_of_relation: relation -> string
val relation_in: class_declaration -> relation
val relations_in: program -> relation list

val extend: relation list -> relation list
