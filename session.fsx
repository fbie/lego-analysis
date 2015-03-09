module Session =

  module Time =
    [<Measure>] type ms
    [<Measure>] type s
    [<Measure>] type min
    let millis t = t * 1.0<ms>
    let secs t = t * 1.0<s>
    let mins t = t * 1.0<min>
    let millisToSecs (t: float<ms>) = t / 1000.0<ms/s>
    let secsToMins (t: float<s>) = t / 60.0<s/min>
    let millisToMins (t: float<ms>) = t |> millisToSecs |> secsToMins

    type Timestamp (time: string, start: string) =
      let time = secs (float time)
      let start = secs (float start)
      member this.elapsed = time - start

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
        | _ -> None

  module Parse =
    let private makeEntry (e: string array) =
      (Time.Timestamp (e.[0], e.[1]), (Action.makeAction e.[2..]))

    let rec private unoption =
      function
        | (ts, None) :: tail -> unoption tail
        | (ts, Some a) :: tail -> (ts, a) :: (unoption tail)
        | [] -> []

    let makeEntries (lines: string array) =
      lines
      |> Seq.map (fun l -> l.Split (';') |> makeEntry)
      |> Seq.toList
      |> unoption

    let readLines filePath = System.IO.File.ReadAllLines(filePath)

    let parseFile filePath =
      filePath
      |> readLines
      |> makeEntries

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
argv
|> List.map Session.Parse.parseFile
|> List.iter (Seq.iter (fun e ->
                        match e with
                        | (_, Session.Action.Duration d) -> printf "%As\n" d
                        | _ -> ()))
