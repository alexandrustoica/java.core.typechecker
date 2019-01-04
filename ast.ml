module AST = struct
	
	type 'a t = Empty | Node of 'a
	
	let return x = Node x
	
	let get monad orElse =
		 match monad with 
		| Empty -> orElse
		| Node node -> node
	
end