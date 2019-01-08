open Context
open Type
open Expression
open Field
open Environment
open ClassFields

let type_of (var: variable) (context: context): system_type =
	match var with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| VarWithName name -> find_in (env_of context) name
	| VarWithField (name, field) ->	(type_of_field 
			(find_by name (fields_in (program_of context) 
				(string_of_type (find_in (env_of context) name)))))	
