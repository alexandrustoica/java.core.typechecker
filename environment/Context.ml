
type context = Context of Program.program * Environment.environment

let program_of = function Context (prog, _) -> prog
let environment_of = function Context (_, env) -> env

let insert_in context record = match context with
	| Context (prog, env) -> 
		Context(prog, (Environment.insert_in env record))

let rec insert_all context records = 
	match records with
	| [] -> context
	| h :: t -> insert_all (insert_in context h) t 
