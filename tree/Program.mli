open Class

type program = Program of class_declaration list
val string_of_program: program -> string