module Time =
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

  type Stamp (time, start) =
    member this.elapsed = if start = 0.0<s> then start else time - start

  let stamp (time: string) (start: string) =
    Stamp (secs (float time), secs (float start))

  let empty =
    Stamp (secs 0.0, secs 0.0)
