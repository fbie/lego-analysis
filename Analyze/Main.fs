module Analyze.Main

open Analyze.Chart
open Analyze.Gaze
open Analyze.Time

let analyze (file: string) =
  let raw = file.Replace(".csv", "-raw.csv") |> Gaze.Raw.parseFile
  let entries = file |> Gaze.Events.parseFile

  let start = entries |> Seq.head |> fst
  let duration = entries |> Seq.last |> fst

  let trunc f =
    Seq.filter (fun x -> let t = f x in t >= start || t <= duration)

  let progress = entries
                 |> Session.progress
  let pupils = raw
               |> trunc (fun x -> x.aT - x.startT)
               |> Session.pupilSize

  Chart.subplot 5 1
  Chart.xlim (float start) (float duration)
  Chart.ylim -0.2 0.2
  Chart.yaxis "Dilation (normalized)"
  Chart.lines "pupils" pupils "alpha=0.5"

  Chart.subplot 5 1
  Chart.xlim (float start) (float duration)
  Chart.ylim 0.0 18.0
  Chart.yaxis "Progress"
  Chart.lines "progress" progress "width=2"

  Chart.subplot 5 1
  Chart.xlim (float start) (float duration)
  Chart.yaxis "Time spent (s)"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (entries |> Session.attention)
  Chart.bars "zoom" (entries |> Session.zoom)
  Chart.bars "rotate" (entries |> Session.rotate)
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim (float start) (float duration)
  Chart.yaxis "Time spent %"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (entries |> Session.tAttention)
  Chart.bars "zoom" (entries |> Session.tZoom)
  Chart.bars "rotate" (entries |> Session.tRotate)
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim (float start) (float duration)
  Chart.yaxis "# Events"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (entries |> Session.nAttention)
  Chart.bars "zoom" (entries |> Session.nZoom)
  Chart.bars "rotate" (entries |> Session.nRotate)
  Chart.legend ""

  Chart.plotdone ""

[<EntryPoint>]
let main argv =
  argv |> Array.iter analyze; 0
