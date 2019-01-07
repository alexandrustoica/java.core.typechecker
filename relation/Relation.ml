open Type
open Class
open Program

type relation = Relation of system_type * system_type

let head (relation: relation) : system_type =
	match relation with
	| Relation (it, _) -> it

let tail (relation: relation) : system_type =
	match relation with
	| Relation (_, it) -> it

let compare (left: relation) (right: relation) : bool =
	match (left, right) with
	| (Relation (ll, rl), Relation (lr, rr)) ->
			(Type.compare ll lr) && (Type.compare rl rr)

let string_of_relation (relation: relation): string =
	match relation with
	| Relation (base, super) ->
			(string_of_type base) ^ " <: " ^ (string_of_type super)

let relation_in
		(cls: class_declaration) : relation = Relation(
		UserDefinedType(name_of_class cls),
		UserDefinedType(super_of_class cls))

let relations_in (prog: program) : relation list =	match prog with
	| Program classes -> List.map (fun it -> (relation_in it)) classes

let tails_for (hd: system_type) (_in: relation list): system_type list =
	List.map (fun relation -> (tail relation))
		(List.filter (fun it -> Type.compare (head it) hd) _in)

let related_with
		(relation: relation)
		(_in: relation list) : relation list =
	match relation with
	| Relation (left, right) ->
			List.map (fun it -> Relation(left, it)) (tails_for right _in)

let rec extend_relations
		(storage: relation list)
		(relations: relation list)
		(acc: relation list) : relation list =
	match relations with
	| [] -> acc
	| h:: t -> let new_relations = (related_with h storage)
			in extend_relations storage t (acc @ [h] @ new_relations)

let extend (relations: relation list) : relation list =
	extend_relations relations relations []
