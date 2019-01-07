open Program
open Relation
open Type


let related_with
		(typ: system_type)
		(in_prog: program): system_type list =
	(tails_for typ (extend (relations_in in_prog)))


let is_related
		(base: system_type)
		(super: system_type)
		(_in: program): bool =
	let relation = Relation(base, super)
	in let relations = (extend (relations_in _in))
	in (List.exists (fun it -> Relation.compare it relation) relations) || (Type.compare base super)
