open Type
include module type of ExpressionTypes

val string_of_expression: expression -> string
val string_of_operation: operation -> string
