module Session =

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

  type Action =
    | Tracking of string * int
    | Zoom of     float
    | Rotation of float
    | Session of  string
    | Dwell of    string
    | Next of     int * int
    | Previous of int * int
    | Duration of float<s>

  let makeAction =
    function
      | [|"Tracking state changed"; active; value|] -> Some (Tracking (active, int value))
      | [|"Zoom changed"; delta|] -> Some (Zoom (float delta))
      | [|"Rotation changed"; angle|] -> Some (Rotation (float angle))
      | [|"Session ID"; id|] -> Some (Session id)
      | [|"Dwell time exceeded"; direction|] -> Some (Dwell direction)
      | [|"Next"; step; substep|] -> Some (Next (int step, int substep))
      | [|"Previous"; step; substep|] -> Some (Previous (int step, int substep))
      | [|"Time to completion"; ms|] -> Some (Duration ((float ms) |> millis |> millisToSecs))
      | _ -> None

  let makeEntry (e: string array) =
    (Timestamp (e.[0], e.[1]), (makeAction e.[2..]))

  let makeEntries (lines: string array) =
    lines
    |> Seq.map (fun l -> l.Split (';') |> makeEntry)
    |> Seq.filter (fun e -> match e with
                              | (_, Some _) -> true
                              | _ -> false)

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
|> List.map Session.parseFile
|> List.map Seq.length
|> List.iter (printfn "%d")
