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
  Chart.bars "attention" s.attention
  Chart.bars "zoom" s.zoom
  Chart.bars "rotate" s.rotate
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "Time spent %"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" s.tAttention
  Chart.bars "zoom" s.tZoom
  Chart.bars "rotate" s.tRotate
  Chart.legend ""

  Chart.subplot 5 1
  Chart.xlim start duration
  Chart.yaxis "# Events"
  Chart.xaxis "Time (s)"
  Chart.bars "attention" s.nAttention
  Chart.bars "zoom" s.nZoom
  Chart.bars "rotate" s.nRotate
  Chart.legend ""

  Chart.plotdone ""

let aggregate files =
  let avg = Session.mkAveraged files

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Time (s)"
  Chart.lines "attention" avg.attention ""
  Chart.lines "zoom" avg.zoom ""
  Chart.lines "rotate" avg.rotate ""
  Chart.lines "duration" avg.duration ""
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "% time"
  Chart.lines "attention" avg.tAttention ""
  Chart.lines "zoom" avg.tZoom ""
  Chart.lines "rotate" avg.tRotate ""
  Chart.legend ""

  Chart.subplot 3 1
  Chart.xlim 0.0 18.0
  Chart.yaxis "Num events"
  Chart.lines "attention" avg.nAttention ""
  Chart.lines "zoom" avg.nZoom ""
  Chart.lines "rotate" avg.nRotate ""
  Chart.lines "regression" avg.regression ""
  Chart.legend ""

  Chart.plotdone ""

[<EntryPoint>]
let main argv =
  //  argv |> Array.iter plot; 0
  aggregate argv; 0
