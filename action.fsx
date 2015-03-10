#load "time.fsx"
open Time

module Action =
  type Action =
    | Tracking of string * int
    | Zoom of     float
    | Rotation of float
    | Session of  string
    | Dwell of    string
    | Next of     int * int
    | Previous of int * int
    | Duration of float<Time.s>
    | Done

  let makeAction =
    function
      | [|"Tracking state changed"; active; value|] -> Some (Tracking (active, int value))
      | [|"Zoom changed"; delta|] -> Some (Zoom (float delta))
      | [|"Rotation changed"; angle|] -> Some (Rotation (float angle))
      | [|"Session ID"; id|] -> Some (Session id)
      | [|"Dwell time exceeded"; direction|] -> Some (Dwell direction)
      | [|"Next step"; step; substep|] -> Some (Next (int step, int substep))
      | [|"Previous step"; step; substep|] -> Some (Previous (int step, int substep))
      | [|"Time to completion"; ms|] -> Some (Duration ((float ms) |> Time.millis |> Time.millisToSecs))
      | [|"Done"|] -> Some Done
      | _ -> None

  let private maybeEntry (e: string array) =
    if e.Length > 1
    then ((Time.stamp e.[0] e.[1]), (makeAction e.[2..]))
    else (Time.stamp 0.0 0.0, None)

  let rec normalize =
    function
      | [] -> []
      | (_, Action.Done) :: t -> []
      | h :: t -> h :: normalize t

  let rec private unoption =
    function
      | (ts, None) :: t -> unoption t
      | (ts, Some a) :: t -> (ts, a) :: (unoption t)
      | [] -> []

  let makeEntries (lines: string array) =
    lines
    |> Seq.map (fun l -> l.Split (';') |> maybeEntry)
    |> Seq.toList
    |> unoption

  let readLines filePath =
    System.IO.File.ReadAllLines(filePath)

  let parseFile filePath =
    filePath
    |> readLines
    |> makeEntries
    |> Seq.toList
    |> normalize
