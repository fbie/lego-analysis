module Analyze.Session

open Analyze.Action
open Analyze.Time
open Analyze.Raw
open Analyze.Waves

module private Util =
  (* Find median point in time between two neighboring events. *)
  let center (a: (float<_> * _) seq) =
    a
    |> Seq.pairwise
    |> Seq.map (fun (x, y) -> fst x + (fst y - fst x) / 2.0)

  let std aSeq =
    if not (Seq.isEmpty aSeq) then
      let avg = Seq.average aSeq
      (Seq.fold (fun s x -> s + (x - avg) ** 2.0) 0.0 aSeq |> sqrt) / (Seq.length >> float) aSeq
    else
      0.0

  let sstd aSeq =
    if Seq.length aSeq > 1 then
      let avg = Seq.average aSeq
      (Seq.fold (fun s x -> s + (x - avg) ** 2.0) 0.0 aSeq |> sqrt) / ((Seq.length >> float) aSeq - 1.0)
    else
      0.0

  let sem aSeq =
    let n = Seq.length aSeq
    if n > 0 then
      sstd aSeq / float n
    else
      0.0

module private Progress =
  let private asFloat =
    function
      | Next (a, b)
      | Previous (a, b) -> if b = 0 then float a else float a - 1.0 + (float b) / 10.0
      | _ -> failwith "Cannot compute steps as float for non-step event."

  let steps a =
    a
    |> Seq.filter (fun x ->
                   match snd x with
                   | Next _
                   | Previous _ -> true
                   | _ -> false)

  let prog a =
    a
    |> steps
    |> (fun s -> seq { yield Seq.head s; yield! s; yield Seq.last s })
    |> Seq.pairwise
    |> Seq.collect (fun (x, y) ->
                    let s = y |> snd |> asFloat
                    seq { yield (fst x, s); yield (fst y, s) })

let progress = Progress.prog

module private Events =
  let private aT = 0.8<s> (* Delay between look-away to logging. *)
  let private rT = 4.0<s> (* A rotation of 360°. *)
  let private zT = 0.2<s> (* For a change of 11 x change. *)

  (* Partition event timeline by steps. *)
  let partition a =
    let step =
      function
        | Next _
        | Previous _ -> true
        | _ -> false
    a
    |> Seq.scan (fun (s, _) t -> if step (snd t) then (s + 1, t) else (s, t)) (0, a |> Seq.head)
    |> Seq.groupBy (fun (x, _) -> x)
    |> Seq.map (fun (_, s) -> s |> Seq.map (fun (_, x) -> x))

  (* Zip some events with steps. *)
  let zip a f =
    let s = a
            |> Progress.steps
            |> Util.center
    let t = a
            |> f
            |> Seq.skip 1 (* Skip one to align with steps *)
    Seq.zip s t

  (* Compute attention time per step. *)
  let attention a =
    partition a
    |> Seq.pairwise
    |> Seq.map (fun (x, y) -> seq { yield! x; yield y |> Seq.head })
    |> Seq.map (fun x ->
                x
                |> Seq.filter (fun x ->
                               match snd x with
                               | Tracking _
                               | Next _
                               | Previous _ -> true
                               | _ -> false)
                |> Seq.pairwise
                |> Seq.map (fun x ->
                            match x with
                            | ((t, Tracking b), (s, _)) -> if b then s - t else -aT
                            | ((t, _), (s, Tracking b)) when not b -> s - t
                            | ((t, (Next _ | Previous _)), (s, (Next _ | Previous _))) -> s - t
                            | _ -> 0.0<s>)
                |> Seq.sum)

  let zoom a =
    partition a
    |> Seq.map (fun x ->
                x
                |> Seq.sumBy (fun x ->
                              match snd x with
                              | Zoom _ -> zT
                              | _ -> 0.0<s>))

  let rotate a =
    partition a
    |> Seq.map (fun x ->
                x
                |> Seq.sumBy (fun x ->
                              match snd x with
                              | Rotation d -> rT
                              | _ -> 0.0<s>))

let attention a =
  Events.zip a Events.attention

let zoom a =
  Events.zip a Events.zoom

let rotate a =
  Events.zip a Events.rotate

let normalize (l: seq<'a * float>) =
  let _, m = l |> Seq.maxBy (fun (_, y) -> y)
  l |> Seq.map (fun (x, y) -> x, y / m)

let interpolate aSeq =
  Seq.delay (fun () ->
             aSeq
             |> Seq.fold (fun l t ->
                          (if not l.IsEmpty && snd t = 0.0 then (fst t, snd l.Head) else t) :: l) []
             |> List.rev
             |> List.toSeq)

let filterOutliers aSeq =
  let lim = 2.0 * (aSeq |> Seq.map (fun x -> snd x) |> Util.std)
  aSeq |> Seq.filter (fun x -> snd x >= lim)

let applyR f aSeq =
  Seq.zip (aSeq |> Array.map (fun x -> fst x)) (aSeq |> Array.map (fun x -> snd x) |> f)

let pupilSize aSeq =
  aSeq
  |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
  |> interpolate
  |> filterOutliers
  |> Seq.toArray
  |> applyR Waves.d16
  |> normalize
