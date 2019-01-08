
let related_with typ in_program =
	let in_relations = Relation.extend (Relation.relations_in in_program) in
	Relation.tails_for typ in_relations

let is_related left right in_program = 
	let relation = Relation.Relation(left, right) in
	let is_equal = fun it -> Relation.compare it relation
	and relations = (Relation.extend (Relation.relations_in in_program))
	in (List.exists is_equal relations) || (Type.compare left right)

let is_connected left right in_program =
	(is_related left right in_program) || (is_related right left in_program)
