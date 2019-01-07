open Program
open Type
open SubType
open Field
open Class 


let fields_for (typ: string) (in_prog: program) : field_declaration list =
	match in_prog with
	| Program classes ->
			List.flatten (
					List.map (fun it -> (fields_of_class it)) (
							List.find_all (fun cls -> (name_of_class cls) = typ) classes))

let fields_for (typ: system_type) (in_prog: program) : field_declaration list =
	match typ with
	| UserDefinedType value -> fields_for value in_prog
	| _ -> []

let fields_in (prog: program) (class_name: string) : field_declaration list =
	match class_name with
	| "Object" -> []
	| _ -> List.flatten (List.map (fun typ -> (fields_for typ prog)) (
							(SubType.super_types_of (UserDefinedType(class_name)) prog) @
							[UserDefinedType(class_name)]))
