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

  let rec duration =
    function
      | [] -> 0.0<s>
      | (ts: Stamp, _) :: [] -> ts.elapsed
      | (_, Action.Duration d) :: t -> d
      | _ :: t -> duration t

  let rec next =
    function
      | [] -> 0
      | (_, Next _) :: t -> 1 + next t
      | _ :: t -> next t

  let rec prev =
    function
      | [] -> 0
      | (_, Previous _) :: t -> 1 + prev t
      | _ :: t -> prev t

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
    let _, m = l |> List.maxBy (fun (_, y) -> y)
    l |> List.map (fun (x, y) -> x, y / m)

  let rec zoom =
    function
      | (ts: Stamp, Action.Zoom _) :: t -> float ts.elapsed :: zoom t
      | _ :: t -> zoom t
      | [] -> []

  let rec rotate =
    function
      | (ts: Stamp, Action.Rotation _) :: t -> float ts.elapsed :: rotate t
      | _ :: t -> rotate t
      | [] -> []

  let toPoints h elems =
    elems |> List.map (fun e -> (e, h))

  let attention actions =
    let rec intervals last actions =
      match actions with
        | (ts: Stamp, Tracking b) :: t -> match b with
                                            | true -> intervals ts.elapsed t
                                            | false -> seq {for p in int last .. int ts.elapsed -> float p} :: intervals 0.0<s> t
        | (ts, a) :: [] -> if last <> 0.0<s> then seq {for p in int last .. int ts.elapsed -> float p} :: [] else []
        | _ :: t -> intervals last t
        | [] -> []
    actions
    |> intervals 0.0<s>
    |> List.fold (fun s t -> s @ (t |> Seq.toList)) []

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let entries = argv.Head |> Action.parseFile

let gp = new GnuPlot()
gp.Set(style = Style(fill = Pattern 1))
gp.SendCommand "set xlabel 'Time (s)'"
gp.SendCommand "set ylabel 'Progress (normalized)'"
gp.SendCommand "set term png" // svg"
gp.SendCommand "set output 'plot'"
[ Series.Points (title="attention", data=(entries |> Session.attention |> (Session.toPoints 0.3)))
  Series.Points (title="zoom", data=(entries |> Session.zoom |> (Session.toPoints 0.2)))
  Series.Points (title="rotate", data=(entries |> Session.rotate |> (Session.toPoints 0.1)))
  Series.Lines (title="progress", weight=2, data=(entries |> Session.progress2 |> Session.normalize))
  ]
|> gp.Plot
