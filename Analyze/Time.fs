module Analyze.Time

[<Measure>] type ms
[<Measure>] type s
[<Measure>] type min

let millis t =
  t * 1.0<ms>

let secs t =
  t * 1.0<s>

let mins t =
  t * 1.0<min>

let millisToSecs (t: float<ms>) =
  t / 1000.0<ms/s>

let secsToMins (t: float<s>) =
  t / 60.0<s/min>

let millisToMins (t: float<ms>) =
  t |> millisToSecs |> secsToMins

let stamp (time: string) (start: string) =
  let t = (float >> secs) time
  let s = (float >> secs) start
  if s <> 0.0<s> then t - s else s

let empty = 0.0<s>
