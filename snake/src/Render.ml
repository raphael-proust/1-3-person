open Gg
open Vg
let float = float_of_int
let pi = 3.14159265358979312

let rotate ?(center=V2.v 0. 0.) r image = image >>
	I.move (V2.neg center) >>
	I.rot (float r *. pi /. 2.) >>
	I.move center

let cell =
	I.const Color.black >>
	I.cut (P.empty >> P.rect (Box2.v (P2.v 0. 0.) (Size2.v 1. 1.)))

let apple =
	I.const Color.black >>
	I.cut (P.empty >> P.circle (P2.v 0.5 0.5) 0.5)

let head_straight =
	I.const Color.black >>
	I.cut
		(P.empty >>
		P.sub (P2.v 0. 0.) >>
		P.line (P2.v 0.5 0.5) >>
		P.line (P2.v 0. 1.) >>
		P.close)

let head_right =
	I.const Color.black >>
	I.cut
		(P.empty >>
		P.sub (P2.v 0. 0.) >>
		P.line (P2.v 1. 0.) >>
		P.line (P2.v 0. 1.) >>
		P.close)

let head_left =
	I.const Color.black >>
	I.cut
		(P.empty >>
		P.sub (P2.v 0. 0.) >>
		P.line (P2.v 1. 1.) >>
		P.line (P2.v 0. 1.) >>
		P.close)

let display_raw (world: Model.world) =
	let place (x,y) image = image >>
		I.move (V2.v (float x) (float y))
	in
	let add_apples apples image =
		List.fold_left (fun img xy -> img >> I.blend (place xy apple))
			image
			apples
	in
	let add_cells cells image =
		List.fold_left (fun img xy -> img >> I.blend (place xy cell))
			image
			cells
	in
	let add_head xy t d image =
		image >> I.blend (
			(match t with
				| None -> head_straight
				| Some Input.L -> head_left
				| Some Input.R -> head_right) >>
			rotate ~center:(V2.v 0.5 0.5) (match d with
				| Model.R -> 0
				| Model.U -> 1
				| Model.L -> 2
				| Model.D -> 3) >>
			place xy
		)
	in
	I.void >>
	add_apples world.Model.apples >>
	add_cells world.Model.cells >>
	add_head world.Model.position world.Model.turn world.Model.direction >>
	let width = float (fst world.Model.dimensions) in
	let height = float (snd world.Model.dimensions) in
	I.scale (V2.v (1. /. width) (1. /. height))

let embed image =
	let delta = 0.1 in
	let unit = 1. in
	let kw = 0.2 in
	let key x y path = path >>
		P.rect (Box2.v
			(P2.v
				(delta +. (x *. (kw +. delta)))
				~-.(2. *. delta +. (2. *. y *. delta)))
			(Size2.v kw delta)
		)
	in

	image >>
	I.blend (
		I.const Color.black >>
		I.cut ~area:`Aeo (
			P.empty >>
			(* contour *)
			P.rect (Box2.v
				(P2.v ~-.delta ~-.(delta +. unit))
				(Size2.v (delta +. unit +. delta) (delta +. (2. *. unit) +. delta))
			)	>>
			(* screen *)
			P.rect (Box2.v (P2.v 0. 0.) (Size2.v 1. 1.)) >>
			(* keys *)
			key 0. 0. >> key 1. 0. >> key 2. 0. >>
			key 0. 1. >> key 1. 1. >> key 2. 1. >>
			key 0. 2. >> key 1. 2. >> key 2. 2. >>
			key 0. 3. >> key 1. 3. >> key 2. 3.
		)
	)

let fst_person (world:Model.world) image =
	image >>

	embed >>

	(* Center on the snake's head *)
	I.move (V2.v
		(0.5 -. ((0.5 +. float (fst world.Model.position)) /. 25.))
		(0.5 -. ((0.5 +. float (snd world.Model.position)) /. 25.))
	) >>

	(* Put the snake's head on P2.o, scale, rotate, put back in the middle *)
	I.move (V2.v ~-.0.5 ~-.0.5) >>
	I.scale (V2.v (1. /. 4.2) (1. /. 4.2)) >>


	(* Rotate so as to always go up: center, rotate, recenter *)
	rotate
	(match Model.(world.turn, world.direction) with
		| None, Model.L | Some Input.L, Model.U | Some Input.R, Model.D -> 3
		| None, Model.R | Some Input.R, Model.U | Some Input.L, Model.D -> 1
		| None, Model.D | Some Input.R, Model.R | Some Input.L, Model.L -> 2
		| None, Model.U | Some Input.R, Model.L | Some Input.L, Model.R -> 0
	) >>

	I.move (V2.v 0.5 0.5)


let display world =
	I.const Color.white >>
	I.blend (
		display_raw world >>
		fst_person world
	)

