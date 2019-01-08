open Program
open Environment
open Record

type context = Context of program * environment

let program_of (context: context): program =
	match context with
	| Context (program, _) -> program

let env_of (context: context): environment =
	match context with
	| Context (_, env) -> env

let insert_in (context: context) (record: record): context =
	match context with
	| Context (prog, env) -> Context(prog, (Environment.insert_in env record))
