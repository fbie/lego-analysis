module Analyze.Main

open Analyze.Gaze
open Analyze.Time
open Analyze.Step

type Error<'a, 'b> =
  | Cont of 'a
  | Fail of 'b

let cont x =
  Cont x

let fail e =
  Fail e

let bind f m =
  match m with
    | Cont x -> f x
    | Fail e -> fail e

let (>>=) m f =
  bind f m

let (>=>) f g =
  f >> (bind g)

let tryWith f m =
  try
    f m |> cont
  with
    | e -> fail e

let (!?) f =
  tryWith f

let readl =
  !? Events.parseFile
  >=> !? Step.mkSteps
  >=> !? Step.group
  >=> !? (Seq.map Step.mkCsv)
  >=> !? Step.reduce

[<EntryPoint>]
let main argv =
  printfn "%s" Csl.header
  Seq.map readl argv
  |> Seq.choose (function
                 | Cont c -> Some c
                 | Fail e -> eprintfn "%s" e.Message; None)
  |> Step.transpose
  |> Seq.map Step.average
  |> Step.cat
  |> printfn "%s"
  0
