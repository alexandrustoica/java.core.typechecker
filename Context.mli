open Program
open Environment
open Record

type context = Context of program * environment

val program_of: context -> program
val env_of: context -> environment
val insert_in: context -> record -> context