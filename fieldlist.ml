open Tree.Tree
open SystemType.Type
open SubType
open Field.Field

module FieldList = struct
	
	let fields_of (cls: class_declaration) : field_declaration list =
		match cls with
		| ClassDeclaration (_, fields, _) -> fields
		| InheritanceDeclaration(_, _, fields, _) -> fields
	
	let class_name_of (cls: class_declaration) : string =
		match cls with
		| ClassDeclaration (name, _, _) -> name
		| InheritanceDeclaration (name, _, _, _) -> name
	
	let fields_for (typ: string) (in_prog: program) : field_declaration list =
		match in_prog with
		| Program classes ->
				List.flatten (
						List.map (fun it -> (fields_of it)) (
								List.find_all (fun cls -> (class_name_of cls) = typ) classes))
	
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
	
end