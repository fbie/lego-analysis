module Analyze.Session

open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw
open Analyze.Waves

module private Util =
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
  let asFloat =
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
    let s = a |> Seq.choose (fun (t, x) ->
                             match x with
                             | Next _
                             | Previous _ -> Some t
                             | _ -> None)
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

  let private forPartitions a f =
    partition a
    |> Seq.map (fun s -> f s)

  let nAttention a =
    forPartitions a (Seq.sumBy (fun x ->
                                match snd x with
                                | Tracking _ -> 1.0
                                | _ -> 0.0))
  let zoom a =
    forPartitions a (Seq.sumBy (fun x ->
                              match snd x with
                              | Zoom _ -> zT
                              | _ -> 0.0<s>))

  let nZoom a =
    forPartitions a (Seq.sumBy (fun x ->
                                  match snd x with
                                  | Zoom _ -> 1.0
                                  | _ -> 0.0))
  let rotate a =
    forPartitions a (Seq.choose (fun x ->
                                 match snd x with
                                 | Rotation d -> Some d
                                 | _ -> None))
    |> Seq.map (fun s -> s
                         |> Seq.pairwise
                         |> Seq.sumBy (fun (x, y) -> abs (x - y) * rT / 360.0))
  let nRotate a =
    forPartitions a (Seq.sumBy (fun x ->
                                match snd x with
                                | Rotation _ -> 1.0
                                | _ -> 0.0))

  let pTime a e =
    let s = e |> Seq.choose (fun x ->
                             match snd x with
                             | Next _
                             | Previous _ -> Some x
                             | _ -> None)
              |> Seq.pairwise
              |> Seq.map (fun (x, y) -> fst y - fst x)
    s
    |> Seq.map2 (fun (x, y) z -> (x, y / z)) a
    |> Seq.truncate (Seq.length s)

let attention a =
  Events.zip a Events.attention

let nAttention a =
  Events.zip a Events.nAttention

let tAttention a =
  Events.pTime (attention a) a

let zoom a =
  Events.zip a Events.zoom

let nZoom a =
  Events.zip a Events.nZoom

let tZoom a =
  Events.pTime (zoom a) a

let rotate a =
  Events.zip a Events.rotate

let nRotate a =
  Events.zip a Events.nRotate

let tRotate a =
  Events.pTime (zoom a) a

module Dilation =
  let private normalize (l: seq<'a * float>) =
    let _, m = l |> Seq.maxBy (fun (_, y) -> y)
    l |> Seq.map (fun (x, y) -> x, y / m)

  let private interpolate a =
    Seq.delay (fun () ->
               a
               |> Seq.fold (fun l t ->
                            (if not l.IsEmpty && snd t = 0.0 then (fst t, snd l.Head) else t) :: l) []
               |> List.rev
               |> List.toSeq)

  let private filterOutliers a =
    let lim = 2.0 * (a |> Seq.map (fun x -> snd x) |> Util.std)
    a |> Seq.filter (fun x -> snd x >= lim)

  let private applyR f a =
    Seq.zip (a |> Array.map (fun x -> fst x)) (a |> Array.map (fun x -> snd x) |> f)

  let pupilSize a =
    a
    |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    |> interpolate
    |> filterOutliers
    |> Seq.toArray
    |> applyR Waves.d16
    |> normalize

let pupilSize a = Dilation.pupilSize a

module Aggregate =
  let perStep f a =
    a
    |> Events.partition
    |> Seq.groupBy (fun x -> (Seq.head x) |> snd |> Progress.asFloat)
    |> Seq.map (fun x -> f (snd x))
