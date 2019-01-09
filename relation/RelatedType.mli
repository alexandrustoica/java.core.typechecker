
(** Returns a list of related type with given type within a program. *)
val related_with: Type.system_type -> Program.program -> Type.system_type list

(** Checks if two types are related based on inheritance within a program. *)
val is_related: Type.system_type -> Type.system_type -> Program.program -> bool

(** Checks if exists a relationship between two types in program. *)
val is_connected: Type.system_type -> 
	Type.system_type -> Program.program -> bool

val are_connected: Type.system_type list ->
	Type.system_type list -> Program.program -> bool