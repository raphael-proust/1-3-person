
(* TODO: a non-constant world *)
let init = { Model.
	dimensions = (25,25);
	apples = [(18,12);];
	cells = [(13,12);(13,11);];
	position = (13,13);
	direction = Model.U;
	turn = None;
}

let empty = { init with Model.
	apples = [];
	cells = [];
	position = (0,0);
	turn = None;
}

type event =
	| Tick
	| Input of Input.t

let tick, kick =
	let (tick, kick) = React.E.create () in
	let clock =
		React.E.map (fun _ ->
			React.E.map (fun _ ->
					kick Tick
				)
				(Useri.Time.tick 0.5)
			)
		tick in
	Useri.App.sink_event clock;
	(tick, kick)

let moves =
	React.E.select [
		React.E.stamp (Useri.Key.up (`Arrow `Left)) (Input Input.L);
		React.E.stamp (Useri.Key.up (`Arrow `Right)) (Input Input.R);
	]

let world =
	React.S.fold
		(fun world e -> match e with
			| Tick -> begin match Model.step world with
				| Model.W w -> w
				| Model.GameOver -> empty
			end
			| Input i -> Model.turn world i
		)
		init
		(React.E.select [tick; moves;])

let setup () =
	let display =
		let img = React.S.l1 Render.display world in
		React.S.Pair.pair Useri.Surface.size img
	in
    let render_display r _ (size, img) =
      let renderable = `Image (size, Gg.Box2.unit, img) in
      ignore (Vg.Vgr.render r renderable);
      ()
    in
    let c = Useri_jsoo.Surface.Handle.to_js (Useri.Surface.handle ()) in
    let r = Vg.Vgr.create (Vgr_htmlc.target ~resize:false c) `Other in
	 (* TODO: interrupt on gameOver event *)
	 Useri.Surface.set_refresher (React.S.changes world);
    Useri.App.sink_event (React.S.sample (render_display r) Useri.Surface.refresh display);
    ()

let clear () =
	let div = Dom_html.getElementById "snake" in
	let nodes = Dom.list_of_nodeList (div ## childNodes) in
	List.iter (Dom.removeChild div) nodes

let start () =
	clear ();
	let canvas = Dom_html.(createCanvas document) in
	let div = Dom_html.getElementById "snake" in
	Dom.appendChild div canvas;
	let handle = Useri_jsoo.Surface.Handle.of_js canvas in
	let surface = Useri.Surface.create ~kind:`Other ~handle () in
	let key_target = Some (div :> Dom_html.eventTarget Js.t) in
	Useri_jsoo.Key.set_event_target key_target;
	match Useri.App.init ~surface () with
	| `Error e -> Printf.eprintf "%s" e; exit 1
	| `Ok () -> setup (); kick Tick; Useri.App.run ()

let () = start ()
