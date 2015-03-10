#r "packages/FnuPlot.0.1.1-beta/lib/net40/FnuPlot.dll"
open FnuPlot
open System
open System.Drawing

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
      member this.elapsed = if start = 0.0<s> then start else time - start

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

  module Parse =
    let private maybeEntry (e: string array) =
      if e.Length > 1 then (Time.Timestamp (e.[0], e.[1]), (Action.makeAction e.[2..])) else (Time.Timestamp ("0", "0"), None)

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

  module Analyze =
    let print prefix x =
      printfn "%s=%A" prefix x

    let rec normalize =
      function
        | [] -> []
        | (_, Action.Done) :: t -> []
        | h :: t -> h :: normalize t

    let rec duration =
      function
        | [] -> 0.0<Time.s>
        | (ts: Time.Timestamp, _) :: [] -> ts.elapsed
        | (_, Action.Duration d) :: t -> d
        | _ :: t -> duration t

    let rec next =
      function
        | [] -> 0
        | (_, Action.Next _) :: t -> 1 + next t
        | _ :: t -> next t

    let rec prev =
      function
        | [] -> 0
        | (_, Action.Previous _) :: t -> 1 + prev t
        | _ :: t -> prev t

    let private toFloat s ss =
      if ss = 0 then float s else float s - 1.0 + (float ss) / 10.0

    let rec progress =
      function
        | (ts: Time.Timestamp, Action.Next (s, ss)) :: t -> (float ts.elapsed, toFloat s ss) :: progress t
        | (ts, Action.Previous (s, ss)) :: t -> (float ts.elapsed, toFloat s ss ) :: progress t
        | [] -> []
        | _ :: t -> progress t

    let rec zoom =
      function
        | (ts: Time.Timestamp, Action.Zoom _) :: t -> ts.elapsed :: zoom t
        | _ :: t -> zoom t
        | [] -> []

    let rec rotate =
      function
        | (ts: Time.Timestamp, Action.Rotation _) :: t -> ts.elapsed :: rotate t
        | _ :: t -> rotate t
        | [] -> []

    let hist interval elems =
      let rec sum interval last elems =
        match elems with
          | [] -> 0.0
          | elapsed :: t -> if elapsed - last > interval then 0.0 else 1.0 + (sum interval last t)
      let rec f interval last elems =
        match elems with
          | [] -> []
          | elapsed :: t -> if elapsed - last > interval
                                 then (float elapsed, sum interval elapsed t) :: (f interval elapsed t)
                                 else f interval last t
      f interval 0.0<Time.s> elems

    let hist5s =
      hist 5.0<Time.s>

    let rec repeat f i =
      if i = 1 then f else f >> repeat f (i - 1)

    let rec offset i vals =
      match vals with
        | [] -> []
        | (x, y) :: t -> (x + i, y) :: offset i t

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let entries = argv.Head
              |> Session.Parse.parseFile
              |> Seq.toList
              |> Session.Analyze.normalize

let gp = new GnuPlot()
gp.Set(style = Style(fill = Pattern 100))
gp.Set(output = Output(Png "plot"))
[ Series.Impulses (title="zoom", weight=3, data=(entries
                                                 |> Session.Analyze.zoom
                                                 |> Session.Analyze.hist5s))
  Series.Impulses (title="rotate", weight=3, data=(entries
                                                   |> Session.Analyze.rotate
                                                   |> Session.Analyze.hist5s
                                                   |> Session.Analyze.offset 2.5))
  Series.Lines (title="progress", weight=2, data=(entries |> Session.Analyze.progress))
  ]
|> gp.Plot
