open Expression.Expression
open Typ.Type
open Ast.AST


let type_of_variable (variable: variable) =
	match variable with
	| KNull -> NullType
	| KInt _ -> PrimitiveType(CoreInt)
	| KFloat _ -> PrimitiveType(CoreFloat)
	| KBool _ -> PrimitiveType(CoreBool)
	| _ -> NullType 

let type_of_expression (expression: expression) =
	match expression with
	| Void -> PrimitiveType(CoreUnit)
	| Var variable -> (type_of_variable variable)  
	| _ -> PrimitiveType(CoreInt)
