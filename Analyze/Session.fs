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

module private Attention =
  (* Add reversed fst s to snd s. *)
  let private cat s =
    List.rev (fst s) :: snd s

  (* Construct a new tuple and propagate last state. *)
  let private con t s =
    let l = s |> fst |> List.head |> snd
    ([(t, l)], cat ((t , l) :: fst s, snd s))

  (* Find all tracking lost events and compute a list of tracking events per step. *)
  let private group a =
    a
    |> Seq.fold (fun s t ->
                 match t with
                 | t', Tracking b -> (t', b) :: (fst s), snd s
                 | t', Next _
                 | t', Previous _ -> con t' s
                 | _ -> s) ([], [])
    |> cat
    |> List.rev
    |> Seq.ofList

  (* Compute attention time per step. *)
  let attention a =
    group a
    |> Seq.map (fun x ->
                x
                |> Seq.pairwise
                |> Seq.map (fun (y, z) ->
                            match y with
                            | t, true -> fst z - t
                            | t, false -> 0.0<s>)
                |> Seq.sum)

let attention a =
  let s = a |> Progress.steps |> Util.center
  let t = Attention.attention a
  Seq.zip s t

let normalize (l: seq<'a * float>) =
  let _, m = l |> Seq.maxBy (fun (_, y) -> y)
  l |> Seq.map (fun (x, y) -> x, y / m)

let private timestamps f =
  Seq.choose (fun (t, a) -> if f a then Some t else None)

let zoom aSeq =
  aSeq |> timestamps (fun a -> match a with | Zoom _ -> true | _ -> false)

let rotate aSeq =
  aSeq |> timestamps (fun a -> match a with | Rotation _ -> true | _ -> false)

let toPoints h aSeq =
  aSeq |> Seq.map (fun e -> (e, h))

let derivate aSeq =
  aSeq |> Seq.pairwise |> Seq.map (fun (n, m) -> fst m, snd m - snd n)

let assertTemporalOrdering s =
  s
  |> Seq.pairwise
  |> Seq.iter (fun (n, m) -> if not (fst n < fst m) then failwith "Temporal ordering violated")
  s

let interpolate aSeq =
  Seq.delay (fun () ->
             aSeq
             |> Seq.fold (fun l t ->
                          (if not l.IsEmpty && snd t = 0.0 then (fst t, snd l.Head) else t) :: l) []
             |> List.rev
             |> List.toSeq)

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

let filterOutliers aSeq =
  let lim = 2.0 * (aSeq |> Seq.map (fun x -> snd x) |> std)
  aSeq |> Seq.filter (fun x -> snd x >= lim)

let applyR f aSeq =
  Seq.zip (aSeq |> Array.map (fun x -> fst x)) (aSeq |> Array.map (fun x -> snd x) |> f)

let pupilSize aSeq =
  aSeq
  |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
  |> interpolate
  |> filterOutliers
  |> Seq.toArray
  |> applyR Waves.d7
  |> normalize
