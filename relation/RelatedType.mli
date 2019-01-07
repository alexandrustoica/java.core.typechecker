open Type
open Program
open Relation

(** Returns a list of related type with given type within a program. *)
val related_with: system_type -> program -> system_type list

(** Checks if two types are related based on inheritance within a program. *)
val is_related: system_type -> system_type -> program -> bool
