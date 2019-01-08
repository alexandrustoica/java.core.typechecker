
type context = Context of Program.program * Environment.environment

val program_of: context -> Program.program
val environment_of: context -> Environment.environment
val insert_in: context -> Record.record -> context
