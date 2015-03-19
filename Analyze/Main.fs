module Analyze.Main

open Analyze

let analyze (file: string) =
  let raw = file.Replace(".csv", "-raw.csv") |> Raw.parse
  let entries = file |> Action.parseFile

  let start = entries |> Seq.head |> fst
  let duration = entries |> Seq.last |> fst

  let trunc f =
    Seq.choose (fun x -> let t = f x in if t >= start || t <= duration then Some x else None)

  try
    let progress = entries
                   |> Seq.toList
                   |> Session.progress2
                   |> Seq.map (fun x -> fst x, snd x / 17.0)
    let pupils = raw
                 |> trunc (fun x -> x.aT - x.startT)
                 |> Session.pupilSize

    Chart.subplot 3 1
    Chart.xlim (float start) (float duration)
    Chart.ylim -0.5 0.5
    Chart.yaxis "Dilation (normalized)"
    Chart.lines "pupils" pupils "alpha=0.5"

    Chart.subplot 3 1
    Chart.xlim (float start) (float duration)
    Chart.ylim 0.0 1.1
    Chart.yaxis "Progress (normalized)"
    Chart.lines "progress" progress "width=2"

    Chart.subplot 3 1
    Chart.xlim (float start) (float duration)
    Chart.ylim 0.0 1.0
    Chart.xaxis "Time (s)"
    Chart.points "attention" ((Seq.toList >> Session.attention >> (Session.toPoints 0.5) >> trunc fst) entries) ""
    Chart.points "zoom" ((Session.zoom >> (Session.toPoints 0.3) >> trunc fst) entries) ""
    Chart.points "rotate" ((Session.rotate >> (Session.toPoints 0.1) >> trunc fst) entries) ""

    Chart.legend ""
    Chart.plotdone ""
  with
    | _ -> printfn ""

[<EntryPoint>]
let main argv =
  argv |> Array.iter analyze; 0
