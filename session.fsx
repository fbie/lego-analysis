#load "time.fsx"
#load "action.fsx"
#load "raw.fsx"
#load "waves.fsx"

open System
open System.Drawing

open Action.Action
open Time.Time
open Raw.Raw
open Waves

module Session =
  let private toFloat s ss =
    if ss = 0 then float s else float s - 1.0 + (float ss) / 10.0

  let rec progress =
    function
      | (ts, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t -> (ts, toFloat s ss) :: progress t
      | [] -> []
      | _ :: t -> progress t

  let progress2 aSeq =
    let rec steps last elems =
      match elems with
        | (ts: float<s>, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t ->
          let n = (toFloat s ss)
          (ts, last) :: (ts, n) :: steps n t
        | [] -> []
        | _ :: t -> steps last t
    steps 0.0 aSeq |> List.toSeq

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

  let attention actions =
    Seq.delay (fun () ->
      let rec intervals last actions =
        match actions with
          | (ts, Tracking b) :: t -> match b with
                                     | true -> intervals ts t
                                     | false -> seq { for p in int last .. int ts -> secs (float p) } :: intervals 0.0<s> t
          | (ts, a) :: [] -> if last <> 0.0<s> then seq { for p in int last .. int ts -> secs (float p) } :: [] else []
          | _ :: t -> intervals last t
          | [] -> []
      actions
      |> intervals 0.0<s>
      |> List.fold (fun s t -> s @ (t |> Seq.toList)) []
      |> List.toSeq)

  let derivate aSeq =
    aSeq |> Seq.pairwise |> Seq.map (fun (n, m) -> fst m, snd m - snd n)

  let assertTemporalOrdering s =
    s |> Seq.pairwise |> Seq.iter (fun (n, m) -> if not (fst n < fst m) then failwith "Temporal ordering violated"); s

  let interpolate aSeq =
    Seq.delay (fun () ->
               aSeq |> Seq.fold (fun l t ->
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
