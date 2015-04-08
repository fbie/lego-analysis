module Analyze.Step

open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw

open Extensions

(* A single step and all the events occured while it lasted. *)
type Step =
  { idx: float; events: (float<s> * Action) seq }

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
  |> Seq.sumBy (function
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
  |> Seq.sumBy (fun (x, y) -> abs (y - x) * 4.0<s> / 360.0)

(* Compute the time the user spent looking at the screen. *)
let attention s =
  Seq.choose (fun x ->
              match snd x with
              | Tracking b -> Some (fst x, b)
              | _ -> None) s.events
  |> Seq.pairwise
  |> Seq.sumBy (fun (a, b) -> if snd b then fst b - fst a + 0.8<s> else 0.0<s>)
  |> (-) (duration s)

let private isStep =
  function
    | Next _ | Previous _ -> true
    | _ -> false

let private isReg =
  function
    | Previous _ -> true
    | _ -> false

let private idxFromStep x s =
  match s with
    | Next (s, ss) | Previous (s, ss) -> if ss = 0 then float s else float s - 1.0 + (float ss / 10.0)
    | _ -> x

(* Make a step from a state and some events. *)
let private mkStep a =
  let h = Seq.head a |> snd
  let i = idxFromStep 0.0 h
  if not (isStep h) then
    None
  else if isReg h then
    Some { idx = i; events = a }
  else
    Some { idx = i; events = a }

(* Much faster than the recursive, pure version. *)
let rec mark f l =
  let i = ref 0
  seq { for e in l do
          yield !i, e
          if f e then
            i := !i + 1
            yield !i, e }

(* Make steps from a timeline of events. *)
let mkSteps a =
  mark (snd >> isStep) a
  |> Seq.groupBy fst
  |> Seq.map (fun (_, s) -> s |> Seq.map snd)
  |> Seq.choose mkStep

(* Group steps by index and remove group key. *)
let group a =
  Seq.groupBy idx a
  |> Seq.map snd

(* Comma separated line. *)
type Csl =
  { idx: float;
    duration: float<s>;
    attention: float<s>;
    zoom: float<s>;
    rotate: float<s> }

  (* For decorating the CSV file. *)
  static member header = "index;duration;attention;zoom;rotate"

  (* Simply add two lines. *)
  static member add a b =
    let mkIdx a b =
      match a.idx, b.idx with
        | x, 0.0 -> x
        | 0.0, x -> x
        | x, y when x = y -> x
        | _ -> failwith (sprintf "Not allowed to add two steps of different indices: %f and %f!" a.idx b.idx)
    { idx = mkIdx a b;
      duration = a.duration + b.duration;
      attention = a.attention + b.attention;
      zoom = a.zoom + b.zoom;
      rotate = a.rotate + b.rotate }

  static member (+) (a, b) =
    Csl.add a b

  (* Divie a line by some integer. *)
  static member (/) (a, i) =
    { idx = a.idx;
      duration = a.duration / float i;
      attention = a.attention / float i;
      zoom = a.zoom / float i;
      rotate = a.rotate / float i }

  static member DivideByInt (a: Csl) i =
    if i = 0 then a else a / i

  (* The empty line. *)
  static member empty =
    { idx = 0.0;
      duration = 0.0<s>;
      attention = 0.0<s>;
      zoom = 0.0<s>;
      rotate = 0.0<s> }

  static member get_Zero () =
    Csl.empty

let mkCsl a =
  { idx = (idx a);
    duration = (duration a);
    attention = (attention a);
    zoom = (zoom a);
    rotate = (rotate a) }

let mkCsv a =
  Seq.map mkCsl a

(* Sort all steps so that every step is immediately followed
   by the sum of its regressions, if any. *)
let reduce (a: Csl seq seq) =
  Seq.map (fun x -> seq { yield Seq.head x; yield Seq.sum (Seq.skip 1 x) }) a
  |> Seq.concat

let catl l =
  sprintf "%f;%f;%f;%f;%f" l.idx (float l.duration) (float l.attention) (float l.zoom) (float l.rotate)

let cat l =
  Seq.map catl l
  |> String.concat "\n"

let transpose l =
  Seq.map (Seq.mapi (fun i x -> i, x)) l
  |> Seq.concat
  |> Seq.groupBy fst
  |> Seq.map (fun (i, s) -> Seq.map snd s)

let average (csv: Csl seq) =
  let l = Seq.length csv
  if l <= 0 then
    Csl.empty
  else
    (Seq.sum csv) / l
