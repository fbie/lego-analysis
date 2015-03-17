#load "session.fsx"

open Action
open Raw
open Session

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let chart kind title (data: ('a *'b) seq) =
  let ps = Seq.map (fun (x, y) -> "(" + (string x) + "," + (string y) + ")") >> Seq.toList >> String.concat ","
  printfn "cmd=%s;label=%s;data=%s;%s" kind title (ps data)

let points =
  chart "points"

let lines =
  chart "lines"

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

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let file = argv.Head
let raw = file.Replace(".csv", "-raw.csv") |> Raw.parse
let entries = file |> Action.parseFile
let duration = entries |> Session.duration
let trunc = Session.truncate duration
let progress = entries
               |> Seq.toList
               |> Session.progress2
               |> Session.normalize
let pupils = raw
             |> Session.pupilSize
             |> trunc

subplot 3 1
xlim 0.0 (float duration)
ylim -0.5 0.5
yaxis "Dilation (normalized)"
lines "pupils" pupils "alpha=0.5"

subplot 3 1
xlim 0.0 (float duration)
ylim 0.0 1.1
yaxis "Progress (normalized)"
lines "progress" progress "width=2"

subplot 3 1
xlim 0.0 (float duration)
ylim 0.0 1.0
xaxis "Time (s)"
points "attention" ((Seq.toList >> Session.attention >> (Session.toPoints 0.5) >> trunc) entries) ""
points "zoom" ((Session.zoom >> (Session.toPoints 0.3) >> trunc) entries) ""
points "rotate" ((Session.rotate >> (Session.toPoints 0.1) >> trunc) entries) ""

legend ""
