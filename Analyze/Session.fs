module Analyze.Session

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

  let apply f g h i a =
    Seq.zip (a |> Seq.map (fun x -> f x) |> g) (a |> Seq.map (fun x -> h x) |> i)

  let applyR f a =
    apply fst id snd f a

  let applyL f a =
    apply fst f snd id a

  let swap a =
    apply snd id fst id

  let map f g h i a =
    Seq.map (fun x -> (f >> g) x , (h >> i) x) a

  let mapR f a =
    map fst id snd f a

  let mapL f a =
    map fst f snd id a

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
  let partition a =
    let step =
      function
        | Next _
        | Previous _ -> true
        | _ -> false
    a
    |> Seq.scan (fun (s, _) t -> if step (snd t) then (s + 1, t) else (s, t)) (0, a |> Seq.head)
    |> Seq.groupBy fst
    |> Seq.map (fun (_, s) -> s |> Seq.map snd)

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
    let (_, lim) = (a |> Seq.map snd |> Stats.std id)
    a |> Seq.filter (fun x -> snd x >= 2.0 * lim)

  let pupilSize a =
    a
    |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    |> interpolate
    |> filterOutliers
    |> Util.applyR (Array.ofSeq >> Waves.d16)
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

  let private mapStep b (a: (float<_> * float<_>) seq) =
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

  let nAttention a =
    aggregate nAttention a

  let tAttention a =
    aggregate tAttention a

  let zoom a =
    aggregate zoom a

  let nZoom a =
    aggregate nZoom a

  let tZoom a =
    aggregate tZoom a

  let rotate a =
    aggregate rotate a

  let nRotate a =
    aggregate nRotate a

  let tRotate a =
    aggregate tRotate a

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
  { events: (float<s> * Action) seq; raw: Raw seq}
  member private this.wrap f = lazy (f this.events)
  member this.attention = this.wrap attention
  member this.nAttention = this.wrap nAttention
  member this.tAttention = this.wrap tAttention
  member this.zoom = this.wrap zoom
  member this.nZoom = this.wrap nZoom
  member this.tZoom = this.wrap tZoom
  member this.rotate = this.wrap rotate
  member this.nRotate = this.wrap nRotate
  member this.tRotate = this.wrap tRotate
  member this.start = lazy (this.events |> Seq.head |> fst)
  member this.duration = lazy (this.events |> Seq.last |> fst)
  member this.dilation = lazy (
    let s = this.start.Force ()
    let d = this.duration.Force ()
    this.raw
    |> Seq.filter (fun x -> let t = x.aT - x.startT in t >= s || t <= d)
    |> pupilSize)
  member this.progress = lazy (progress this.events)

let mkSession file =
  let e = file |> Events.parseFile
  let r = file.Replace(".csv", "-raw.csv") |> Raw.parseFile
  { events = e; raw = r }

type Aggregated =
  { s: Session }
  member private this.wrap (f: Lazy<'a>) = lazy (Aggregate.aggregateLazy this.s.events (f.Force()))
  member this.attention = this.wrap this.s.attention
  member this.nAttention = this.wrap this.s.nAttention
  member this.tAttention = this.wrap this.s.tAttention
  member this.zoom = this.wrap this.s.zoom
  member this.nZoom = this.wrap this.s.nZoom
  member this.tZoom = this.wrap this.s.tZoom
  member this.rotate = this.wrap this.s.rotate
  member this.nRotate = this.wrap this.s.nRotate
  member this.tRotate = this.wrap this.s.tRotate
  member this.duration = lazy (Aggregate.duration this.s.events)
  member this.regression = lazy (Aggregate.regressions this.s.events)

let mkAggregated file =
    { s = (mkSession file) }

let private group a =
  a
  |> Seq.groupBy fst
  |> Seq.map (fun x -> fst x, snd x |> Seq.map snd)

type Averaged =
  { ags: Aggregated seq }

  member private this.group (f: Aggregated -> Lazy<('a * float<_>) seq>) =
    Seq.map (fun x -> async { return (f x).Force () }) this.ags
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.concat
    |> group

  member private this.avg (a: ('a * float<_> seq) seq) =
    Seq.map (fun x -> async { return fst x, Seq.average (snd x) }) a
    |> Async.Parallel
    |> Async.RunSynchronously

  member private this.weightened (f: Aggregated -> Lazy<_>) a =
    let w = Seq.map (fun (x: Aggregated) -> x.duration.Force() |> Seq.skip 1) a
    let v = Seq.map (fun x -> (f x).Force()) a
    Seq.map2 (fun x y -> Seq.map2 (fun i j -> fst i, snd i * snd j) x y ) w v
    |> Seq.concat
    |> group
    |> Util.mapR Seq.sum
    |> Seq.map2 (fun x y -> fst y, snd y / snd x) (Seq.concat w |> group |> Util.mapR Seq.sum)

  member this.attention = (this.group >> this.avg) (fun x -> x.attention)
  member this.nAttention = (this.group >> this.avg) (fun x -> x.nAttention)
  member this.tAttention = this.weightened (fun x -> x.tAttention) this.ags

  member this.zoom = (this.group >> this.avg) (fun x -> x.zoom)
  member this.nZoom = (this.group >> this.avg) (fun x -> x.nZoom)
  member this.tZoom = this.weightened (fun x -> x.tZoom) this.ags

  member this.rotate = (this.group >> this.avg) (fun x -> x.rotate)
  member this.nRotate = (this.group >> this.avg) (fun x -> x.nRotate)
  member this.tRotate = this.weightened (fun x -> x.tRotate) this.ags

  member this.duration = (this.group >> this.avg) (fun x -> x.duration)
  member this.regression = (this.group >> this.avg) (fun x -> x.regression)

let mkAveraged files =
  { ags = Seq.map (fun x -> mkAggregated x) files }
