module Analyze.Step

open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw
open Extensions

type Step = { idx: int; events: (float<s> * Action) seq }

let idx s =
  s.idx

let duration s =
  let diff i =
    Seq.last i - Seq.head i
  Seq.map fst s.events
  |> diff

let private events s =
  Seq.map snd s.events

let zoom s =
  events s
  |> Seq.sumBy (fun x ->
                match x with
                | Zoom _ -> 0.2<s>
                | _ -> 0.0<s>)

let rotate s =
  let rot =
    function
      | Rotation d -> Some d
      | _ -> None
  events s
  |> Seq.choose rot
  |> Seq.pairwise
  |> Seq.sumBy (fun (x, y) -> abs (x - y) * 4.0<s> / 360.0)

let attention s =
  let att =
    function
      | Tracking b -> Some b
      | _ -> None
  Seq.applyR (Seq.choose att) s.events
  |> Seq.pairwise
  |> Seq.sumBy (fun (a, b) -> if snd b then fst b - fst a + 0.8<s> else 0.0<s>)
  |> (-) (duration s)

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

let private sIdx m = State.force idx 0 m

let private isStep =
  function
    | Next _ | Previous _ -> true
    | _ -> false

let private isReg =
  function
    | Previous _ -> true
    | _ -> false

let private mkStep s a =
  if not ((snd >> isStep) (Seq.head a)) then
    State.Empty
  else
    if ((snd >> isReg) (Seq.head a)) then
      State.last { idx = sIdx s - 1; events = a }
    else
      State.last { idx = sIdx s + 1; events = a }

let mkSteps a =
  Seq.scan (fun (s, t) x -> if isStep (snd x) then (s + 1, x) else (s, t)) (0, a |> Seq.head) a
  |> Seq.groupBy fst
  |> Seq.map (fun (_, s) -> s |> Seq.map snd)
  |> Seq.scan mkStep State.empty
  |> Seq.choose (fun x -> match x with | State.Last s -> Some s | _ -> None)

let sort a =
  Seq.groupBy idx a
  |> Seq.map snd
  |> Seq.concat
