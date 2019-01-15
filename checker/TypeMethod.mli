
val override_method_of: Method.method_declaration -> 
	Class.class_declaration -> Method.method_declaration option

val is_overriding: Method.method_declaration ->
	 Method.method_declaration -> Context.context -> bool

val type_of: Method.method_declaration -> Context.context -> Type.system_type
