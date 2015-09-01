
type direction =
	| U
	| D
	| L
	| R

type player = {
	position: int * int;
	direction: direction;
	turn: Input.t option;
	cells: (int * int) list;
}
type world = {
	dimensions: int * int;
	blue: player;
	gold: player;
	game_over: bool;
}

let init =
	let player = {
		position= (0,0);
		direction= U;
		turn= None;
		cells= [];
	}
	in
	{
		dimensions = (50,50);
		blue={player with position=(14,14);};
		gold={player with position=(14,35);};
		game_over = false;
	}

let game_over w = { w with
	game_over = true;
}


let step_player p =
	let ndirection = match (p.turn, p.direction) with
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
	let nposition = match ndirection with
		| R -> (fst p.position + 1, snd p.position)
		| L -> (fst p.position - 1, snd p.position)
		| D -> (fst p.position, snd p.position - 1)
		| U -> (fst p.position, snd p.position + 1)
	in
	{p with
		direction=ndirection;
		position=nposition;
		cells=p.position::p.cells;
	}

let turn (w:world) (p:player) (t:Input.t) =
	if w.game_over then
		p
	else
	match (p.turn, t) with
	| (Some Input.L, Input.L) | (Some Input.R, Input.R) -> p
	| (Some Input.L, Input.R) | (Some Input.R, Input.L) ->
		{p with turn=None;}
	| (None, t) -> {p with turn=Some t;}

let turn w (p,t) = match p with
	| Input.Blue -> {w with blue=turn w w.blue t}
	| Input.Gold -> {w with gold=turn w w.gold t}


let inside {dimensions=(xx,yy)} {position=(x,y)} =
	0 < x && x < xx
	&&
	0 < y && y < yy

let step w =
	if w.game_over then
		w
	else

	let nblue = step_player w.blue in
	let ngold = step_player w.gold in

	if List.mem nblue.position w.blue.cells
	|| List.mem nblue.position w.gold.cells
	|| not (inside w nblue)
	then
		(* blue loses *)
		game_over w
	else
	if List.mem ngold.position w.blue.cells
	|| List.mem ngold.position w.gold.cells
	|| not (inside w ngold)
	then
		(* gold loses *)
		game_over w
	else

	{w with
		blue=nblue;
		gold=ngold;
	}


