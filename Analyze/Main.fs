module Analyze.Main

open Analyze.Gaze
open Analyze.Time
open Analyze.Step

module Error =
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

  let tryWith f m =
    try
      f m |> cont
    with
      | e -> fail e

let (>>=) m f =
  Error.bind f m

let (>=>) f g =
  f >> (Error.bind g)

let parse =
  Error.tryWith Events.parseFile

let csl f =
  parse f
  >>= (mkSteps >> Error.cont)

[<EntryPoint>]
let main argv =
  Seq.map csl argv |> ignore
  0
