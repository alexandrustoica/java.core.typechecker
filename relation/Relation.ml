
type relation = Relation of Type.system_type * Type.system_type

let head = function
	| Relation (it, _) -> it

let tail = function
	| Relation (_, it) -> it

let compare left right =
	match (left, right) with
	| Relation (ll, rl), Relation (lr, rr) ->
			(Type.compare ll lr) && (Type.compare rl rr)

let string_of_relation = function
	| Relation (base, super) ->
			(Type.string_of_type base) ^ " <: " ^ (Type.string_of_type super)

let relation_in cls =
	Relation(
		Type.UserDefinedType(Class.name_of_class cls),
		Type.UserDefinedType(Class.super_of_class cls))

let relations_in = function
	| Program.Program classes -> List.map relation_in classes

let tails_for hd within =
	let equal_heads it = Type.compare (head it) hd in
	within |> List.filter equal_heads |> List.map tail

let related_with relation within =
	let create_relation x = fun it -> Relation(x, it) in
	match relation with
	| Relation (left, right) -> within
			|> tails_for right
			|> List.map (create_relation left)

let rec extend_relations relations acc all =
	match relations with
	| [] -> acc
	| h :: t ->
			let new_relations = related_with h all in
			extend_relations t (acc @ [h] @ new_relations) all

let extend relations = extend_relations relations [] relations
