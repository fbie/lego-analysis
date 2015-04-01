module Analyze.Step

open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw

open Extensions

(* A single step and all the events occured while it lasted. *)
type Step =
  { idx: int; events: (float<s> * Action) seq }

let idx s =
  s.idx

(* Compute the difference between the last and
   the first event. *)
let duration s =
  let diff i =
    Seq.last i - Seq.head i
  Seq.map fst s.events
  |> diff

let private events s =
  Seq.map snd s.events

(* Compute the time used to zoom. *)
let zoom s =
  events s
  |> Seq.sumBy (fun x ->
                match x with
                | Zoom _ -> 0.2<s>
                | _ -> 0.0<s>)

(* Compute the time used to rotate.
   NB: The rotation is only reported when the user
   stops rotating. This means that if someone just
   kept rotating, this will never be reported. *)
let rotate s =
  let rot =
    function
      | Rotation d -> Some d
      | _ -> None
  events s
  |> Seq.choose rot
  |> Seq.pairwise
  |> Seq.sumBy (fun (x, y) -> abs (x - y) * 4.0<s> / 360.0)

(* Compute the time the user spent looking at the screen. *)
let attention s =
  let att =
    function
      | Tracking b -> Some b
      | _ -> None
  Seq.applyR (Seq.choose att) s.events
  |> Seq.pairwise
  |> Seq.sumBy (fun (a, b) -> if snd b then fst b - fst a + 0.8<s> else 0.0<s>)
  |> (-) (duration s)

(* Small monad for building steps from event timeline. *)
module private State =
  type State<'a> =
    | Empty
    | Last of 'a

  let empty =
    Empty

  let last v =
    Last v

  let force f x m =
    match m with
      | Empty -> x
      | Last s -> f s

let private sIdx m =
  State.force idx 0 m

let private isStep =
  function
    | Next _ | Previous _ -> true
    | _ -> false

let private isReg =
  function
    | Previous _ -> true
    | _ -> false

(* Make a step from a state and some events. *)
let private mkStep s a =
  if not ((snd >> isStep) (Seq.head a)) then
    State.Empty
  else if ((snd >> isReg) (Seq.head a)) then
    State.last { idx = sIdx s - 1; events = a }
  else
    State.last { idx = sIdx s + 1; events = a }

(* Make steps from a timeline of events. *)
let mkSteps a =
  Seq.scan (fun (s, t) x -> if isStep (snd x) then (s + 1, x) else (s, t)) (0, a |> Seq.head) a
  |> Seq.groupBy fst
  |> Seq.map (fun (_, s) -> s |> Seq.map snd)
  |> Seq.scan mkStep State.empty
  |> Seq.choose (fun x -> match x with | State.Last s -> Some s | _ -> None)

(* Group steps by index and remove group key. *)
let group a =
  Seq.groupBy idx a
  |> Seq.map snd

(* Comma separated line. *)
type Csl =
  { idx: int;
    duration: float<s>;
    attention: float<s>;
    zoom: float<s>;
    rotate: float<s> }

  (* For decorating the CSV file. *)
  static member header = "index;duration;attention;zoom;rotate"

  (* Simply add two lines. *)
  static member (+) (a, b) = { idx = a.idx + b.idx;
                               duration = a.duration + b.duration;
                               attention = a.attention + b.attention;
                               zoom = a.zoom + b.zoom;
                               rotate = a.rotate + b.rotate }

  (* Divie a line by some integer. *)
  static member (/) (a, i) = { idx = a.idx / i;
                               duration = a.duration / float i;
                               attention = a.attention / float i;
                               zoom = a.zoom / float i;
                               rotate = a.rotate / float i }

  (* The empty line. *)
  static member empty = { idx = 0;
                          duration = 0.0<s>;
                          attention = 0.0<s>;
                          zoom = 0.0<s>;
                          rotate = 0.0<s> }

  static member get_Zero () = Csl.empty

let mkCsl a =
  { idx = (idx a);
    duration = (duration a);
    attention = (attention a);
    zoom = (zoom a);
    rotate = (rotate a) }

let mkCsv a =
  Seq.map mkCsl a

(* Sort all steps so that every step is immediately followed
   by its regressions and sum up the regressions. *)
let reduce (a: Csl seq seq) =
  Seq.map (fun x ->
           seq { yield Seq.head x; yield (Seq.sum (Seq.skip 1 x)) }) a
  |> Seq.concat
