open Gg
open Vg
let float = float_of_int
let pi = 3.14159265358979312

let rotate ?(center=V2.v 0. 0.) r image = image >>
	I.move (V2.neg center) >>
	I.rot (float r *. pi /. 2.) >>
	I.move center

let cell color =
	I.const color >>
	I.cut (P.empty >> P.rect (Box2.v (P2.v 0.1 0.1) (Size2.v 0.8 0.8)))

let wall = cell Color.black

let display_raw (world: Model.world) =
	let place (x,y) image = image >>
		I.move (V2.v (float x) (float y))
	in
	let add_player player color image =
		List.fold_left (fun img xy -> img >> I.blend (place xy (cell color)))
			image
			(player.Model.position :: player.Model.cells)
	in

	I.void >>
	add_player world.Model.blue (Color.v_srgbi 125 249 255) >>
	add_player world.Model.gold (Color.v_srgbi 255 215 0) >>
	let width = float (fst world.Model.dimensions) in
	let height = float (snd world.Model.dimensions) in
	I.scale (V2.v (1. /. width) (1. /. height))

let lcd_screen image =
	I.const (Color.v 0. 0. 0. 1.) >>
	I.cut (P.empty >> P.rect (Box2.v (P2.v 0. 0.) (Size2.v 1.0 1.0))) >>
	I.blend image

let embed image =
	image

let fst_person (world:Model.world) (player:Model.player) image =
	image >>

	embed >>

	(* Center on the player's poistion *)
	I.move (V2.v
		(0.5 -. ((0.5 +. float (fst player.Model.position)) /. 25.))
		(0.5 -. ((0.5 +. float (snd player.Model.position)) /. 25.))
	) >>

	(* Put the players's position on P2.o, scale, rotate, put back in the middle *)
	I.move (V2.v ~-.0.5 ~-.0.5) >>
	I.scale (V2.v (1. /. 6.) (1. /. 6.)) >>

	(* Rotate so as to always go up: center, rotate, recenter *)
	rotate
	(match Model.(player.turn, player.direction) with
		| None, Model.L | Some Input.L, Model.U | Some Input.R, Model.D -> 3
		| None, Model.R | Some Input.R, Model.U | Some Input.L, Model.D -> 1
		| None, Model.D | Some Input.R, Model.R | Some Input.L, Model.L -> 2
		| None, Model.U | Some Input.R, Model.L | Some Input.L, Model.R -> 0
	)


let display world =
	I.const Color.white >>
	I.blend (
		let image =
			display_raw world >>
			lcd_screen
		in
		image >>
		fst_person world world.Model.blue >>
		I.move (V2.v 0.5 0.5)
		>> I.blend (
			image >>
			fst_person world world.Model.gold >>
			I.move (V2.v 0.5 1.5)
		)
	)

