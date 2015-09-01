
type direction =
	| U
	| D
	| L
	| R

type world = {
	dimensions: int * int;
	apples: (int * int) list;
	cells:  (int * int) list;
	position: int * int;
	direction: direction;
	turn: Input.t option;
	game_over: bool;
}

let init = {
	dimensions = (25,25);
	apples = [(18,12);];
	cells = [(13,12);(13,11);];
	position = (13,13);
	direction = U;
	turn = None;
	game_over = false;
}

let game_over w = { w with
	game_over = true;
}


let rec add_apple (w:world) : (int*int) =
	let x = Random.int (fst w.dimensions) in
	let y = Random.int (snd w.dimensions) in
	if List.mem (x,y) w.apples
	|| List.mem (x,y) w.cells
	|| (abs (x - fst w.position) <= 1 && abs (y - snd w.position) <= 1)
	then
		add_apple w
	else
		(x,y)

let step w =
	if w.game_over then
		w
	else

	let posmod a s =
		let m = a mod s in
		if m < 0 then s + m else m
	in

	let ndirection = match (w.turn, w.direction) with
		| None, d -> d
		| Some Input.L, U -> L
		| Some Input.R, U -> R
		| Some Input.L, D -> R
		| Some Input.R, D -> L
		| Some Input.L, R -> U
		| Some Input.R, R -> D
		| Some Input.L, L -> D
		| Some Input.R, L -> U
	in

	let nposition = 
		let (x,y) = match ndirection with
			| R -> (fst w.position + 1, snd w.position)
			| L -> (fst w.position - 1, snd w.position)
			| D -> (fst w.position, snd w.position - 1)
			| U -> (fst w.position, snd w.position + 1)
		in
		(posmod x (fst w.dimensions), posmod y (snd w.dimensions))
	in

	if List.mem nposition w.cells then
		game_over w
	else

	let (ncells, napples) =
		let get_apple (x,y) apples =
			List.fold_left
				(fun (found, acc) (xx,yy) ->
					if found then
						(true, (xx,yy)::acc)
					else
						if x=xx && y=yy then
							(true, acc)
						else
							(false, (xx,yy)::acc)
				)
				(false, [])
				apples
		in
		match get_apple nposition w.apples with
		| (true, apples) ->
			(*NOTE: add_apple is called with the old world. However, nposition
			 * is also the position of the apple just got. So it can't appear
			 * there. *)
			(w.position::w.cells, (add_apple w)::apples)
		| (false, apples) ->
			let rec remove_last = function
				| [] | [_] -> []
				| x::xs -> x::remove_last xs
			in
				(remove_last (w.position::w.cells), apples)
	in

	{w with
		position = nposition;
		direction = ndirection;
		apples = napples;
		cells = ncells;
		turn = None;
	}

let turn (w:world) (t:Input.t) =
	match (w.turn, t) with
	| (Some Input.L, Input.L) | (Some Input.R, Input.R) -> w
	| (Some Input.L, Input.R) | (Some Input.R, Input.L) ->
		{w with turn=None;}
	| (None, t) -> {w with turn=Some t;}


