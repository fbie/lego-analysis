module Analyze.Session

open Extensions
open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw
open Analyze.Waves
open Analyze.Stats

open System.Collections.Concurrent

module Util =
  let memoize f =
    let c = new ConcurrentDictionary<_,Lazy<_>>()
    (fun k ->
     match c.TryGetValue k with
     | true, v -> v.Force()
     | _ -> let v = lazy f k in c.[k] <- v
            v.Force())

  [<Measure>] type step

  let asStep =
    function
      | Next (a, b)
      | Previous (a, b) -> (if b = 0 then float a else float a - 1.0 + (float b) / 10.0) * 1.0<step>
      | _ -> failwith "Cannot compute steps as float for non-step event."

module private Progress =
  let prog a =
    a
    |> Seq.filter (fun x ->
                   match snd x with
                   | Next _ | Previous _ -> true
                   | _ -> false)
    |> (fun s -> seq { yield Seq.head s; yield! s; yield Seq.last s })
    |> Seq.pairwise
    |> Seq.collect (fun (x, y) ->
                    let s = y |> snd |> Util.asStep
                    seq { yield (fst x, s); yield (fst y, s) })

let progress = Progress.prog

module private Events =
  let private aT = 0.8<s> (* Delay between look-away to logging. *)
  let private rT = 4.0<s> (* A rotation of 360°. *)
  let private zT = 0.2<s> (* For a change of 11 x change. *)

  (* Partition event timeline by steps. *)
  let partition =
    let step =
      function
        | Next _
        | Previous _ -> true
        | _ -> false
    Util.memoize (fun a ->
                  Seq.scan (fun (s, _) t -> if step (snd t) then (s + 1, t) else (s, t)) (0, a |> Seq.head) a
                  |> Seq.groupBy fst
                  |> Seq.map (fun (_, s) -> s |> Seq.map snd))

  (* Zip some events with steps. *)
  let zip a f =
    let t s = Seq.choose (fun (t, x) ->
                          match x with
                          | Next _ | Previous _ -> Some t
                          | _ -> None) s
    Seq.tuple (id) (id) a
    |> Seq.apply t (f >> Seq.skip 1)

  (* Compute attention time per step. *)
  let attention a =
    partition a
    |> Seq.pairwise
    |> Seq.rLast
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
    partition a |> Seq.map (fun s -> f s)

  let zoom a =
    forPartitions a (Seq.sumBy (fun x ->
                              match snd x with
                              | Zoom _ -> zT
                              | _ -> 0.0<s>))
  let rotate a =
    forPartitions a (Seq.choose (fun x ->
                                 match snd x with
                                 | Rotation d -> Some d
                                 | _ -> None))
    |> Seq.map (fun s -> s
                         |> Seq.pairwise
                         |> Seq.sumBy (fun (x, y) -> abs (x - y) * rT / 360.0))

let attention a =
  Events.zip a Events.attention

let zoom a =
  Events.zip a Events.zoom

let rotate a =
  Events.zip a Events.rotate

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
    let (_, lim) = (a |> Seq.map snd |> Stats.std id)
    a |> Seq.filter (fun x -> snd x >= 2.0 * lim)

  let pupilSize a =
    a
    |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    |> interpolate
    |> filterOutliers
    |> Seq.applyR (Array.ofSeq >> Waves.d16)
    |> normalize

let pupilSize a = Dilation.pupilSize a

module Aggregate =

  let private isStep =
    function
      | Next _ | Previous _ -> true
      | _ -> false

  let private stampToStep =
    Util.memoize (fun a t ->
             let p = a |> Seq.fold (fun s x -> if isStep (snd x) && fst x <= t then Some (snd x) else s) None
             match p with
             | Some x -> Util.asStep x
             | None -> 0.0<Util.step>)

  let private mapStep (b: (_ * Action) seq) (a: (float<_> * float<_>) seq) =
    a
    |> Seq.map (fun x -> stampToStep b (fst x), snd x)
    |> Seq.groupBy fst
    |> Seq.map (fun x -> fst x, snd x |> Seq.sumBy snd)

  let aggregate f a =
    (f >> mapStep a) a

  let aggregateLazy a b =
    mapStep a b

  let attention a =
    aggregate attention a

  let zoom a =
    aggregate zoom a

  let rotate a =
    aggregate rotate a

  let duration a =
    a
    |> Events.partition
    |> Seq.map (fun x -> (Seq.head >> fst) x, (Seq.last >> fst) x - (Seq.head >> fst) x)
    |> mapStep a

  let regressions a =
    a
    |> Seq.filter (fun x ->
                   match snd x with
                   | Next _ | Previous _ -> true
                   | _ -> false)
    |> Seq.pairwise
    |> Seq.map (fun (x, y) ->
                fst x,
                match snd x, snd y with
                | Next _, Previous _ -> 1.0
                | _ -> 0.0)
    |> mapStep a

type Session =
  { events: (float<s> * Action) seq; raw: Lazy<Raw seq>}
  member this.attention = attention this.events
  member this.zoom = zoom this.events
  member this.rotate = rotate this.events
  member this.start = this.events |> Seq.head |> fst
  member this.duration = this.events |> Seq.last |> fst
  member this.dilation =
    let s = this.start
    let d = this.duration
    this.raw.Force()
    |> Seq.filter (fun x -> let t = x.aT - x.startT in t >= s || t <= d)
    |> pupilSize
  member this.progress = progress this.events

let mkSession file =
  let e = file |> Events.parseFile
  let r = file.Replace(".csv", "-raw.csv") |> Raw.parseFile
  { events = e; raw = r }
