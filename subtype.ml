open Program
open Type
open Expression
open Class

type relation = Relation of system_type * system_type

let compare (left: relation) (right: relation) : bool =
	match (left, right) with
	| ((Relation (ll, rl)), (Relation (lr, rr))) ->
			(Type.compare ll lr) &&
			(Type.compare rl rr)

let string_of_relation (relation: relation): string =
	match relation with
	| Relation (base, super) ->
			(string_of_type base) ^ " <: " ^ (string_of_type super)

let relations_in (cls: class_declaration) =
	match cls with
	| ClassDeclaration (base, _, _) ->
			Relation(UserDefinedType(base), UserDefinedType("Object"))
	| InheritanceDeclaration (base, super, _, _) ->
			Relation(UserDefinedType(base), UserDefinedType(super))

let relations_in (prog: program): relation list =
	match prog with
	| Program classes -> List.map (fun it -> (relations_in it)) classes

let super_types_for
		(typ: system_type)
		(in_relations: relation list) =
	List.map (fun it -> match it with Relation (_, r) -> r)
		(List.filter (fun it ->
							match it with
							| Relation (l, _) -> Type.compare l typ)
				in_relations)

let rec extend_relations
		(storage: relation list)
		(relations: relation list)
		(acc: relation list) =
	match relations with
	| [] -> acc
	| h:: t -> let new_relations = (match h with
					| Relation (a, b) -> (List.map (fun it -> Relation(a, it)) (super_types_for b storage)))
			in extend_relations storage t (acc @ [h] @ new_relations)

let extend (relations: relation list): relation list =
	extend_relations relations relations []

let super_types_of
		(typ: system_type)
		(in_prog: program): system_type list =
	(super_types_for typ (extend (relations_in in_prog)))

let is_subtype
		(in_prog: program)
		(base: system_type)
		(super: system_type): bool =
	let relation = Relation(base, super)
	in let relations = (relations_in in_prog)
	in (List.exists (fun it -> compare it relation) (extend relations)) ||
	(Type.compare base super)
