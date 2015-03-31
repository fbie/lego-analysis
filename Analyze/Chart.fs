module Analyze.Chart

open Analyze.Time

(* Generically remove unit of measure from any sequence of float tuples. *)
let private untime (a: (float<_> * float<_>) seq) =
  a |> Seq.map (fun (x, y) -> float x, float y)

let private ps =
  Seq.map (fun (x, y) ->
           "(" + sprintf "%f" x + "," + sprintf "%f" y + ")")
  >> Seq.toList
  >> String.concat ","

let private chart kind title data =
  printfn "cmd=%s;label=%s;data=%s;%s" kind title (data |> untime |> ps)

let points title data =
  chart "points" title data

let lines title data =
  chart "lines" title data

let bars title data width =
  chart "bars" title data (sprintf "width=%f" width)

let errbars title data width =
  let xvals = Seq.map fst data
  let yvals = Seq.map snd data |> Seq.map fst
  let yerr  = Seq.map snd data |> Seq.map snd
  chart "bars" title (Seq.zip xvals yvals) (sprintf "width=%f;yerr=%s" width ((Seq.zip xvals yerr) |> untime |> ps))

let xaxis =
  printfn "cmd=xaxis;label=%s"

let xlim =
  printfn "cmd=xlim;xmin=%f;xmax=%f"

let yaxis =
  printfn "cmd=yaxis;label=%s"

let ylim =
  printfn "cmd=ylim;ymin=%f;ymax=%f"

let legend =
  printfn "cmd=legend;%s"

let subplot =
  printfn "cmd=subplot;width=%d;height=%d"

let plotdone =
  printfn "cmd=done;%s"
