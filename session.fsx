#r "packages/FnuPlot.0.1.1-beta/lib/net40/FnuPlot.dll"

#load "time.fsx"
#load "action.fsx"
#load "raw.fsx"

open FnuPlot
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

  let duration =
    Seq.pick (fun (_, a) -> match a with
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
      | (ts: Stamp, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t -> (float ts.elapsed, toFloat s ss) :: progress t
      | [] -> []
      | _ :: t -> progress t

  let progress2 =
    let rec steps last elems =
      match elems with
        | (ts: Time.Stamp, Next (s, ss)) :: t | (ts, Previous (s, ss)) :: t -> let n = (toFloat s ss) in (float ts.elapsed, last) :: (float ts.elapsed, n) :: steps n t
        | [] -> []
        | _ :: t -> steps last t
    steps 0.0

  let normalize l =
    let _, m = l |> Seq.maxBy (fun (_, y) -> y)
    l |> Seq.map (fun (x, y) -> x, y / m)

  let private timestamps f =
    Seq.choose (fun (t: Stamp, a) -> if f a then Some (float t.elapsed) else None)

  let zoom = timestamps (fun a -> match a with | Zoom _ -> true | _ -> false)
  let rotate = timestamps (fun a -> match a with | Rotation _ -> true | _ -> false)

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

  let interpolate s =
    s |> Seq.fold (fun l t -> (if not l.IsEmpty && snd t = 0.0
                               then (fst t, snd l.Head)
                               else t) :: l) []
      |> List.rev
      |> List.toSeq

  let pupilSize =
    Seq.map (fun (r: Raw) -> float (r.aT - r.startT), (r.leftEye.pupilSize + r.rightEye.pupilSize) / 2.0)
    >> interpolate
    >> derivate
    >> normalize
    >> Seq.map (fun (t, x) -> t, x / 5.0 + 0.4) // Project onto [0.4, 0.6]
    //>> Seq.filter (fun (_, x) -> x <> 0.0) // Remove all zero values

  let truncate t tList =
    tList |> Seq.filter (fun (x, _) -> x < t)

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let file = argv.Head
let rawf = file.Replace(".csv", "-raw.csv")
let raw = rawf |> Raw.parse
let entries = file |> Action.parseFile

let gp = new GnuPlot()
gp.Set(style = Style(fill = Pattern 1))
gp.SendCommand "set xlabel 'Time (s)'"
gp.SendCommand "set ylabel 'Progress (normalized)'"
gp.SendCommand "set term png" // svg"
gp.SendCommand "set output 'plot'"
[ Series.Lines (title="pupil ø", data=(raw |> Session.pupilSize |> Session.truncate (entries |> Session.duration |> float)))
  Series.Points (title="attention", data=(entries |> Session.attention |> (Session.toPoints 0.3)))
  Series.Points (title="zoom", data=(entries |> Session.zoom |> (Session.toPoints 0.2)))
  Series.Points (title="rotate", data=(entries |> Session.rotate |> (Session.toPoints 0.1)))
  Series.Lines (title="progress", weight=2, data=(entries |> Session.progress2 |> Session.normalize))
  ]
|> gp.Plot
