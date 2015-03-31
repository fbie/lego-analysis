module Analyze.Main

open Analyze.Chart
open Analyze.Gaze
open Analyze.Time

let plot (file: string) =
  let s = Session.mkSession file
  let start = float s.start
  let duration = float s.duration

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.ylim -0.2 0.2
  Chart.yaxis "Dilation (normalized)"
  Chart.lines "pupils" s.dilation "alpha=0.5"

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.ylim 0.0 18.0
  Chart.yaxis "Progress"
  Chart.lines "progress" s.progress "width=2"

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "Time spent (s)"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" s.attention 5.0
  Chart.bars "zoom" s.zoom 5.0
  Chart.bars "rotate" s.rotate 5.0
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "Time spent %"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" s.tAttention 5.0
  Chart.bars "zoom" s.tZoom 5.0
  Chart.bars "rotate" s.tRotate 5.0
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "# Events"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" s.nAttention 5.0
  Chart.bars "zoom" s.nZoom 5.0
  Chart.bars "rotate" s.nRotate 5.0
  Chart.legend ""

  Chart.plotdone ""

let aggregate files =
  let avg = Session.mkAveraged files

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Time (s)"
  Chart.bars "attention" avg.attention 0.5
  Chart.bars "zoom" avg.zoom 0.5
  Chart.bars "rotate" avg.rotate 0.5
  Chart.bars "duration" avg.duration 0.5
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "% time"
  Chart.bars "attention" avg.tAttention 0.5
  Chart.bars "zoom" avg.tZoom 0.5
  Chart.bars "rotate" avg.tRotate 0.5
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Num events"
  Chart.bars "attention" avg.nAttention 0.5
  Chart.bars "zoom" avg.nZoom 0.5
  Chart.bars "rotate" avg.nRotate 0.5
  Chart.bars "regression" avg.regression 0.5
  Chart.legend ""

  Chart.plotdone ""

[<EntryPoint>]
let main argv =
  //  argv |> Array.iter plot; 0
  aggregate argv; 0
