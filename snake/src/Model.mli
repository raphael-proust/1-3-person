
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

val init: world
val step: world -> world
val turn: world -> Input.t -> world

