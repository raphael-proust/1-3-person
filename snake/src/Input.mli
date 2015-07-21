
type t =
	| L
	| R

(* Start the clock *)
val kick: unit -> unit

val input: t React.E.t
val ticks: unit React.E.t
