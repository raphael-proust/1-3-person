
type i =
	| Tick
	| Input of (Input.p * Input.t)

let world =
	React.S.fold
		(fun world e -> match e with
			| Tick -> Model.step world
			| Input i -> Model.turn world i
		)
		Model.init
		React.E.(select [
			map (fun x -> Input x) Input.input;
			stamp Input.ticks Tick;
		])


let setup () =
	let display =
		let img = React.S.map Render.display world in
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
	let div = Dom_html.getElementById "tron" in
	let nodes = Dom.list_of_nodeList (div ## childNodes) in
	List.iter (Dom.removeChild div) nodes

let start () =
	clear ();
	let canvas = Dom_html.(createCanvas document) in
	let div = Dom_html.getElementById "tron" in
	Dom.appendChild div canvas;
	let handle = Useri_jsoo.Surface.Handle.of_js canvas in
	let surface = Useri.Surface.create ~kind:`Other ~handle () in
	let key_target = Some (div :> Dom_html.eventTarget Js.t) in
	Useri_jsoo.Key.set_event_target key_target;
	match Useri.App.init ~surface () with
	| `Error e -> Printf.eprintf "%s" e; exit 1
	| `Ok () -> setup (); Input.kick (); Useri.App.run ()

let () = start ()
