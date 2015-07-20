
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
}

type r =
	| W of world
	| GameOver

val step: world -> r

val turn: world -> Input.t -> world

