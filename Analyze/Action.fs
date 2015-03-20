module Analyze.Action

open Analyze.Time

type Action =
  | Tracking of bool
  | Zoom of     float
  | Rotation of float
  | Session of  string
  | Dwell of    string
  | Next of     int * int
  | Previous of int * int
  | Duration of float<s>
  | TutorialEnd
  | Done

let makeAction =
  function
    | [|"Tracking state changed"; _ ; value|] -> Some (Tracking (int value = 111))
    | [|"Zoom changed"; delta|] -> Some (Zoom (float delta))
    | [|"Rotation changed"; angle|] -> Some (Rotation (float angle))
    | [|"Session ID"; id|] -> Some (Session id)
    | [|"Dwell time exceeded"; direction|] -> Some (Dwell direction)
    | [|"Next step"; step; substep|] -> Some (Next (int step, int substep))
    | [|"Previous step"; step; substep|] -> Some (Previous (int step, int substep))
    | [|"Time to completion"; ms|] -> Some (Duration ((float ms) |> millis |> millisToSecs))
    | [|"Tutorial"; "end"|] -> Some TutorialEnd
    | [|"Are you done?"|] -> Some Done
    | _ -> None

let private maybeEntry (e: string array) =
  if e.Length > 1 then
    ((stamp e.[0] e.[1]), (makeAction e.[2..]))
  else
    (empty, None)

let private normalize aSeq =
  Seq.delay (fun () ->
             let bSeq = aSeq |> Seq.skipWhile (fun x ->
                                               match snd x with
                                               | TutorialEnd -> false
                                               | _ -> true)
             match bSeq |> Seq.tryFindIndex (fun x ->
                                             match snd x with
                                             | Done -> true
                                             | _ -> false) with
             | Some i -> bSeq |> Seq.truncate i
             | None -> bSeq)

let makeEntries (lines: string array) =
  let e = lines |> Seq.map (fun l -> l.Split (';') |> maybeEntry)
  Seq.zip (e |> Seq.map (fun x -> fst x)) (e |> Seq.map (fun x -> snd x) |> Seq.choose id)

let readLines filePath =
  System.IO.File.ReadAllLines(filePath)

let parseFile filePath =
  filePath
  |> readLines
  |> makeEntries
  |> normalize
