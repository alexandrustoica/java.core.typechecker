
(** Returns and checks a variable's type within a given context. *)
val type_of: Variable.variable -> Context.context -> Type.system_type

val types_of: Variable.variable list -> Context.context -> 
	Type.system_type list
