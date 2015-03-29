module Analyze.Session

open Analyze.Time
open Analyze.Gaze
open Analyze.Gaze.Events
open Analyze.Gaze.Raw
open Analyze.Waves

module Util =
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

  let apply f g h i a =
    Seq.zip (a |> Seq.map (fun x -> f x) |> g) (a |> Seq.map (fun x -> h x) |> i)

  let applyR f a =
    apply fst id snd f a

  let applyL f a =
    apply fst f snd id a

  let swap a =
    apply snd id fst id

  let map f g h j a =
    Seq.zip ((a |> Seq.map (fun x -> f x)) |> g) ((a |> Seq.map (fun x -> h x)) |> j)

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

  let pupilSize a =
    a
    |> Seq.map (fun (r: Raw) -> (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    |> interpolate
    |> filterOutliers
    |> Util.mapR (Array.ofSeq >> Waves.d16)
    |> normalize

let pupilSize a = Dilation.pupilSize a

module Aggregate =

  let private stampToStep a t =
    a |> Seq.skipWhile (fun x -> fst x <> t) |> Seq.head |> snd |> Util.asStep

  let private mapStep b (a: (float<_> * float<_>) seq) =
    a
    |> Seq.map (fun x -> stampToStep b (fst x), snd x)
    |> Seq.groupBy fst
    |> Seq.map (fun x -> fst x, snd x |> Seq.sumBy (fun x -> snd x))

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

type Averaged =
  { ags: Aggregated seq }
  member private this.avg (f: Aggregated -> Lazy<('a * float<_>) seq>) =
    this.ags
    |> Seq.map (fun x -> (f x).Force ())
    |> Seq.concat
    |> Seq.groupBy fst
    |> Seq.map (fun x -> fst x, snd x |> Seq.averageBy (fun x -> snd x))
  member this.attention = this.avg (fun x -> x.attention)
  member this.nAttention = this.avg (fun x -> x.nAttention)
  member this.tAttention = this.avg (fun x -> x.tAttention)
  member this.zoom = this.avg (fun x -> x.zoom)
  member this.nZoom = this.avg (fun x -> x.nZoom)
  member this.tZoom = this.avg (fun x -> x.tZoom)
  member this.rotate = this.avg (fun x -> x.rotate)
  member this.nRotate = this.avg (fun x -> x.nRotate)
  member this.tRotate = this.avg (fun x -> x.tRotate)
  member this.duration = this.avg (fun x -> x.duration)
  member this.regression = this.avg (fun x -> x.regression)

let mkAveraged files =
  { ags = Seq.map (fun x -> mkAggregated x) files }
