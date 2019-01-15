
type relation = Relation of Type.system_type * Type.system_type

val head: relation -> Type.system_type
val tail: relation -> Type.system_type
val compare: relation -> relation -> bool
val string_of_relation: relation -> string

val tails_for: Type.system_type -> 
	relation list -> Type.system_type list

val relation_in: Class.class_declaration -> relation
val relations_in: Program.program -> relation list

val is_identity: relation -> bool
val extend: relation list -> relation list
