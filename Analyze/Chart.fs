module Analyze.Chart

open Analyze.Time

let chart kind title (data: (float<s> * float) seq) =
  let ps =
    Seq.map (fun (x, y) -> "(" + (string x) + "," + (string y) + ")") >> Seq.toList >> String.concat ","
  printfn "cmd=%s;label=%s;data=%s;%s" kind title (ps data)

let points title data =
  chart "points" title data 

let lines title data =
  chart "lines" title data

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
