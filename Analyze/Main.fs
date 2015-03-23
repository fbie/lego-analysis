module Analyze.Main

open Analyze.Chart
open Analyze.Time

let analyze (file: string) =
  let raw = file.Replace(".csv", "-raw.csv") |> Raw.parse
  let entries = file |> Action.parseFile

  let start = entries |> Seq.head |> fst
  let duration = entries |> Seq.last |> fst

  let trunc f =
    Seq.filter (fun x -> let t = f x in t >= start || t <= duration)

  let progress = entries
                 |> Session.progress
  let pupils = raw
               |> trunc (fun x -> x.aT - x.startT)
               |> Session.pupilSize

  Chart.subplot 3 1
  Chart.xlim (float start) (float duration)
  Chart.ylim -0.2 0.2
  Chart.yaxis "Dilation (normalized)"
  Chart.lines "pupils" pupils "alpha=0.5"

  Chart.subplot 3 1
  Chart.xlim (float start) (float duration)
  Chart.ylim 0.0 18.0
  Chart.yaxis "Progress"
  Chart.lines "progress" progress "width=2"

  Chart.subplot 3 1
  Chart.xlim (float start) (float duration)
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (entries |> Session.attention)
//  Chart.points "zoom" ((Session.zoom >> (Session.toPoints 0.3) >> trunc fst) entries) ""
//  Chart.points "rotate" ((Session.rotate >> (Session.toPoints 0.1) >> trunc fst) entries) ""

  Chart.legend ""
  Chart.plotdone ""

[<EntryPoint>]
let main argv =
  argv |> Array.iter analyze; 0
