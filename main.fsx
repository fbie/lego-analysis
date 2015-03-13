#r "packages/FnuPlot.0.1.1-beta/lib/net40/FnuPlot.dll"

#load "session.fsx"

open FnuPlot

open Action
open Raw
open Session

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let file = argv.Head
let raw = file.Replace(".csv", "-raw.csv") |> Raw.parse
let entries = file |> Action.parseFile
let progress = entries
               |> Session.progress2
               |> Session.normalize
               |> Seq.toList
let pupils = raw
             |> Session.pupilSize
             |> Session.truncate (entries |> Session.duration |> float)
             |> Seq.map (fun t -> let o = progress
                                          |> Seq.choose (fun p -> if fst p <= fst t then Some (snd p) else None)
                                          |> (fun s -> if Seq.isEmpty s then 0.0 else Seq.last s)
                                  fst t, snd t + o)
             |> Seq.toList

let gp = new GnuPlot()
gp.SendCommand "set xlabel 'Time (s)'"
gp.SendCommand "set ylabel 'Progress (normalized)'"
gp.SendCommand "set term png" // svg"
gp.SendCommand "set output 'plot.png'"

[ Series.Points (title="attention", data=(entries |> Session.attention |> Session.toPoints 0.3))
  Series.Points (title="zoom", data=(entries |> Session.zoom |> Session.toPoints 0.2))
  Series.Points (title="rotate", data=(entries |> Session.rotate |> Session.toPoints 0.1))
  Series.Lines (title="pupil ø", data=pupils)
  Series.Lines (title="progress", weight=2, data=progress)
  ]
|> gp.Plot
