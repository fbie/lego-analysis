module Analyze.Main

open Analyze.Chart
open Analyze.Gaze
open Analyze.Time

let plot (file: string) =
  let s = Session.mkSession file
  let start = float (s.start.Force ())
  let duration = float (s.duration.Force ())

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.ylim -0.2 0.2
  Chart.yaxis "Dilation (normalized)"
  Chart.lines "pupils" (s.dilation.Force ()) "alpha=0.5"

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.ylim 0.0 18.0
  Chart.yaxis "Progress"
  Chart.lines "progress" (s.progress.Force ()) "width=2"

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "Time spent (s)"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (s.attention.Force ())
  Chart.bars "zoom" (s.zoom.Force ())
  Chart.bars "rotate" (s.rotate.Force ())
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "Time spent %"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (s.tAttention.Force ())
  Chart.bars "zoom" (s.tZoom.Force ())
  Chart.bars "rotate" (s.tRotate.Force ())
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "# Events"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" (s.nAttention.Force ())
  Chart.bars "zoom" (s.nZoom.Force ())
  Chart.bars "rotate" (s.nRotate.Force ())
  Chart.legend ""

  Chart.plotdone ""

let aggregate files =
  let avg = Session.mkAveraged files

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Time (s)"
  Chart.bars "attention" avg.attention
  Chart.bars "zoom" avg.zoom
  Chart.bars "rotate" avg.rotate
  Chart.bars "duration" avg.duration
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "% time"
  Chart.bars "attention" avg.tAttention
  Chart.bars "zoom" avg.tZoom
  Chart.bars "rotate" avg.tRotate
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Num events"
  Chart.bars "attention" avg.nAttention
  Chart.bars "zoom" avg.nZoom
  Chart.bars "rotate" avg.nRotate
  Chart.bars "regression" avg.regression
  Chart.legend ""

  Chart.plotdone ""

[<EntryPoint>]
let main argv =
  //  argv |> Array.iter plot; 0
  aggregate argv; 0
