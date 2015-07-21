
type t =
	| L
	| R

let rythm = 0.3

let tick, kick =
	let (tick, kick) = React.E.create () in
	let clock =
		React.E.map (fun () ->
			React.E.map (fun (_:float) ->
					kick ()
				)
				(Useri.Time.tick rythm)
			)
		tick in
	Useri.App.sink_event clock;
	(tick, kick)

let kicked = ref false
let kick () =
	if !kicked then
		()
	else
		kick ()

let input =
	React.E.select [
		React.E.stamp (Useri.Key.up (`Arrow `Left)) L;
		React.E.stamp (Useri.Key.up (`Arrow `Right)) R;
	]

let ticks = tick

