open Expression.Expression
open Ast.AST
open Subtype
open Fieldlist.FieldList
open Typexpr

let first_class =
	let first = Assign(VarWithName("c"), Operation(
				IntOperation(IntPlus(Var(VarWithName("a")), Var(VarWithName("b"))))))
	in let second = Assign(VarWithField("this", "f1"), Operation(
				IntOperation(IntPlus(Var(VarWithField("this", "f1")), Var(VarWithName("c"))))))
	in let third = Var(VarWithName("c"))
	in let expression = Compose(first, Compose(second, third))
	in let localBlock = LocalVar(PrimitiveType(CoreInt), VarWithName("c"), expression)
	in let parameters =
		[Parameter(PrimitiveType(CoreInt), "a"); Parameter(PrimitiveType(CoreInt), "b")]
	in let methods = [MethodDeclaration(PrimitiveType(CoreInt), "m1", parameters, localBlock)]
	in let fields = [FieldDeclaration(PrimitiveType(CoreInt), "f1")]
	in ClassDeclaration("A", fields, methods)

let second_class =
	let lastAssignment = Assign(VarWithField("this", "f2"), Var(VarWithName("z")))
	in let last = Compose(lastAssignment, Var(VarWithName("z")))
	in let thenExpr = Assign(VarWithName("z"), New("A", [VarWithName("m")]))
	in let elseExpr = Assign(VarWithName("z"), New("A", [VarWithName("n")]))
	in let ifExpr = If(VarWithName("m"), thenExpr, elseExpr)
	in let mAssign = Assign(VarWithName("m"), Operation(CompareOperation(GT(
						Operation(IntOperation(IntMinus(
									Var(VarWithField("x", "f1")), Var(VarWithField("y", "f1"))))),
						Var(VarWithName("n"))))))
	in let first = Assign(VarWithName("n"),
			Operation(IntOperation(IntMinus(
						Call(VarWithName("x"), "m1", [KInt(1); KInt(2)]),
						Call(VarWithName("y"), "m2", [KInt(2); KInt(1)])))))
	in let localNestedBlock =
		LocalVar(PrimitiveType(CoreBool), VarWithName("m"), Compose(mAssign, ifExpr))
	in let expression = Compose(first, Compose(localNestedBlock, last))
	in let block = LocalVar(PrimitiveType(CoreInt), VarWithName("n"), expression)
	in let funBlock = LocalVar(UserDefinedType("A"), VarWithName("z"), block)
	in let params = [
		Parameter(UserDefinedType("A"), "x");
		Parameter(UserDefinedType("A"), "y")]
	in let methods = [MethodDeclaration(UserDefinedType("A"), "m2", params, funBlock)]
	in let fields = [FieldDeclaration(UserDefinedType("A"), "f2")]
	in InheritanceDeclaration("B", "A", fields, methods)

let third_class =
	let assignmentB = Assign(VarWithName("o1"), New("B", [KInt(0); KNull]))
	in let assignmentA = Assign(VarWithName("o2"), New("A", [KInt(2)]))
	in let assignmentC = Assign(VarWithName("o3"), New("A", [KInt(3)]))
	in let call = Call(VarWithName("o1"), "m2", [VarWithName("o2"); VarWithName("m3")])
	in let last = Assign(VarWithName("o2"), call)
	in let third = LocalVar(UserDefinedType("A"), VarWithName("o3"), Compose(assignmentC, last))
	in let second = LocalVar(UserDefinedType("A"), VarWithName("o2"), Compose(assignmentA, third))
	in let first = LocalVar(UserDefinedType("B"), VarWithName("o1"), Compose(assignmentB, second))
	in let meth = MethodDeclaration(PrimitiveType(CoreUnit), "main", [], first)
	in ClassDeclaration("Main", [], [meth])



let _ =
	let expression = LocalVar(
		PrimitiveType(CoreInt), VarWithName("a"), Var(KInt(3)))
	in print_endline (Typ.Type.string_of_type (
		Typexpr.type_of_expression [] expression))
		
	

(* let () =                                                                                      *)
(* 	let relations = Subtype.relations_in (Program([first_class; second_class; third_class]))    *)
(* 	in print_endline (                                                                          *)
(* 			List.fold_left (fun acc it -> acc ^ "\n" ^ it) ""                                       *)
(* 				(List.map (fun it -> (Subtype.string_of_relation it))                                 *)
(* 						(Subtype.extend relations)))                                                      *)

(* let _ =                                                                                       *)
(* 	print_endline (                                                                             *)
(* 			string_of_bool (                                                                        *)
(* 					Subtype.is_subtype (Program([first_class; second_class; third_class]))              *)
(* 						(UserDefinedType("B"))                                                            *)
(* 						(UserDefinedType("Object"))))                                                     *)

(* let _ = print_endline (string_of_program (Program([first_class; second_class; third_class]))) *)

(* let print_fields (fields: field_declaration list): string =                                   *)
(* 	List.fold_left (fun acc it -> acc ^ "\n" ^ (string_of_field it)) "" fields                  *)

(* let _ =                                                                                       *)
(* 	print_endline                                                                               *)
(* 		(print_fields (fields_in (Program([first_class; second_class; third_class])) "B"))        *)