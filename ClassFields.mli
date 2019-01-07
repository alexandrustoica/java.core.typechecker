(** Compiles The Fields Of A Class *)

open Class
open Field
open Program

(** Returns the fields found in program for given class name. *)
val fields_in: program -> string -> field_declaration list
