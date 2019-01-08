open Expression
open Program
open RelatedType
open ClassFields
open TypeExpression
open Relation
open Type
open Environment 
open Context

let prog = (Program([A.cls; B.cls; M.cls]))

let _ =
	let expression = 
		LocalVar(UserDefinedType("A"), VarWithName("a"), Var(KInt(3)))
	in let context = Context(prog, Environment([]))
	in print_endline (Type.string_of_type (
		TypeExpression.type_of expression context))
	

let () =
	let relations = Relation.relations_in prog
	in print_endline (
			List.fold_left (fun acc it -> acc ^ "\n" ^ it) ""
				(List.map (fun it -> (string_of_relation it)) (extend relations)))


let _= print_endline (string_of_types (Program.user_types_in prog))

let _ = print_endline (
			string_of_bool (
					 RelatedType.is_related 
						(UserDefinedType("B"))
						(UserDefinedType("Object")) prog))

let _ = print_endline (string_of_program prog)

(* let print_fields (fields: field_declaration list): string =                                   *)
(* 	List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_field it)) "" fields                  *)

(* let _ =                                                                                       *)
(* 	print_endline                                                                               *)
(* 		(print_fields (fields_in (Program([first_class; second_class; third_class])) "B"))        *)