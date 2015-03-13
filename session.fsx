#load "time.fsx"
#load "action.fsx"
#load "raw.fsx"

open System
open System.Drawing

open Action
open Time
open Raw

module Session =
  open Action
  open Time
  open Raw

  let print prefix x =
    printfn "%s=%A" prefix x

  let duration aSeq =
    aSeq |> Seq.pick (fun (_, a) -> match a with
                                      | Duration d -> Some d
                                      | _ -> None)

  let next aSeq =
    let f s (_, a) =
      match a with
        | Next _ -> 1 + s
        | _ -> s
    Seq.fold f 0 aSeq

  let prev aSeq =
    let f s (_, a) =
      match a with
        | Previous _ -> 1 + s
        | _ -> s
    Seq.fold f 0 aSeq

  let private toFloat s ss =
    if ss = 0 then float s else float s - 1.0 + (float ss) / 10.0

  let rec progress =
    function
      | (ts: Stamp, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t ->
          (float ts.elapsed, toFloat s ss) :: progress t
      | [] -> []
      | _ :: t -> progress t

  let progress2 =
    let rec steps last elems =
      match elems with
        | (ts: Time.Stamp, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t ->
            let n = (toFloat s ss) in (float ts.elapsed, last) :: (float ts.elapsed, n) :: steps n t
        | [] -> []
        | _ :: t -> steps last t
    steps 0.0

  let normalize l =
    let _, m = l |> Seq.maxBy (fun (_, y) -> y)
    l |> Seq.map (fun (x, y) -> x, y / m)

  let private timestamps f =
    Seq.choose (fun (t: Stamp, a) -> if f a then Some (float t.elapsed) else None)

  let zoom aSeq =
    aSeq |> timestamps (fun a -> match a with | Zoom _ -> true | _ -> false)

  let rotate aSeq =
    aSeq |> timestamps (fun a -> match a with | Rotation _ -> true | _ -> false)

  let toPoints h elems =
    elems |> Seq.map (fun e -> (e, h))

  let attention actions =
    let rec intervals last actions =
      match actions with
        | (ts: Stamp, Tracking b) :: t -> match b with
                                            | true -> intervals ts.elapsed t
                                            | false -> seq { for p in int last .. int ts.elapsed -> float p } :: intervals 0.0<s> t
        | (ts, a) :: [] -> if last <> 0.0<s> then seq { for p in int last .. int ts.elapsed -> float p } :: [] else []
        | _ :: t -> intervals last t
        | [] -> []
    actions
    |> intervals 0.0<s>
    |> List.fold (fun s t -> s @ (t |> Seq.toList)) []

  let derivate aSeq =
    aSeq |> Seq.pairwise |> Seq.map (fun (n, m) -> fst m, snd m - snd n)

  let assertTemporalOrdering s =
    ignore (s |> Seq.pairwise |> Seq.map (fun n m -> if not (fst n < fst m) then failwith "Temporal ordering violated")); s

  let interpolate s =
    s |> Seq.fold (fun l t -> (if not l.IsEmpty && snd t = 0.0
                               then (fst t, snd l.Head)
                               else t) :: l) []
      |> List.rev
      |> assertTemporalOrdering
      |> List.toSeq

  let pupilSize aSeq =
    aSeq
    |> Seq.map (fun (r: Raw) -> float (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    |> interpolate
    |> derivate
    |> normalize
    |> Seq.map (fun (t, x) -> t, x / 10.0) // Project onto [0.4, 0.6]

  let truncate t =
    Seq.filter (fun (x, _) -> x < t)
