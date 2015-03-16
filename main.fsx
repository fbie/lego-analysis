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
  printfn "cmd=%s;title=%s;data=%s;%s" kind title (ps data)

let points =
  chart "points"

let lines =
  chart "lines"

let xaxis =
  printfn "cmd=xaxis;label=%s"

let yaxis =
  printfn "cmd=yaxis;label=%s"

let show =
  printfn "";

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let file = argv.Head
let raw = file.Replace(".csv", "-raw.csv") |> Raw.parse
let entries = file |> Action.parseFile
let progress = entries
               |> Seq.toList
               |> Session.progress2
               |> Session.normalize
let pupils = raw
             |> Session.pupilSize
             |> Session.truncate (entries |> Session.duration)
             |> Seq.map (fun t -> let o = progress
                                          |> Seq.choose (fun p -> if fst p <= fst t then Some (snd p) else None)
                                          |> (fun s -> if Seq.isEmpty s then 0.0 else Seq.last s)
                                  fst t, snd t + o)

xaxis "Time (s)"
yaxis "Progress (normalized)"

points "attention" ((Seq.toList >> Session.attention >> (Session.toPoints 0.3)) entries) ""
points "zoom" ((Session.zoom >> (Session.toPoints 0.2)) entries) ""
points "rotate" ((Session.rotate >> (Session.toPoints 0.1)) entries) ""

lines "pupils" pupils "alpha=0.5"
lines "progress" progress "width=2"
