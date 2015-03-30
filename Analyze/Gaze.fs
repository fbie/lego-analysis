module Analyze.Gaze

open Analyze.Time
open FSharp.Data
open FSharp.Data.CsvExtensions

module Events =
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
    let m = ((stamp e.[0] e.[1]), (makeAction e.[2..]))
    match snd m with
      | Some x -> Some (fst m, x)
      | _ -> None

  let private normalize aSeq =
    let bSeq = aSeq |> Seq.skipWhile (fun x ->
                                      match snd x with
                                      | TutorialEnd -> false
                                      | _ -> true)
    match bSeq |> Seq.tryFindIndex (fun x ->
                                    match snd x with
                                    | Done -> true
                                    | _ -> false) with
      | Some i -> bSeq |> Seq.truncate i
      | None -> bSeq

  let makeEntries (lines: string array) =
    lines
    |> Seq.map (fun l -> l.Split (';') |> maybeEntry)
    |> Seq.choose id

  let readLines filePath =
    System.IO.File.ReadAllLines(filePath)

  let parseFile filePath =
    filePath
    |> readLines
    |> makeEntries
    |> normalize

module Raw =
  type Vec2D =
    { x: float; y: float}
    static member (+) (v1, v2) =
      { x = v1.x + v2.x; y = v1.y + v2.y }
    static member (-) (v1, v2) =
      { x = v1.x - v2.x; y = v1.y - v2.y }
    static member (*) (v, k)
      = { x = v.x * k; y = v.y * k }
    static member (/) (v, k)
      = { x = v.x / k; y = v.y / k }

  type Eye =
    { rawPos: Vec2D; avgPos: Vec2D; pupilSize: float; pupilCenter: Vec2D }

  type Raw =
    { eT: float<s>; dT: float<s>; aT: float<s>;
      fixated: bool; state: string;
      rawPos: Vec2D; avgPos: Vec2D;
      leftEye: Eye; rightEye: Eye;
      startT: float<s>; }

  let private toEye lr (row: CsvRow) =
    let raw = { x = row.[lr + "Rwx"].AsFloat(); y = row.[lr + "Rwy"].AsFloat() }
    let avg = { x = row.[lr + "Avx"].AsFloat(); y = row.[lr + "Avy"].AsFloat() }
    let center = { x = row.[lr + "Cx"].AsFloat(); y = row.[lr + "Cy"].AsFloat() }
    { rawPos = raw; avgPos = avg; pupilSize = row.[lr + "PSz"].AsFloat();  pupilCenter = center}

  let private toRaw (row: CsvRow) =
    try
      let left  = toEye "L" row
      let right = toEye "R" row
      let raw = { x = row?Rwx.AsFloat(); y = row?Rwy.AsFloat() };
      let avg = { x = row?Avx.AsFloat(); y = row?Avy.AsFloat() };
      Some {eT = secs (row?eT.AsFloat()); dT = secs (row?dT.AsFloat()); aT = secs (row?aT.AsFloat());
            fixated = row?Fix = "F"; state = row?State;
            rawPos = raw; avgPos = avg;
            leftEye = left; rightEye = right;
            startT = secs (row?startT.AsFloat())}
    with
      | _ -> None

  let private load (csv: string) =
    CsvFile.Load(csv, separators=";", ignoreErrors=true)

  let parseFile csv =
    lazy (let raw = load csv in raw.Rows |> Seq.map toRaw |> Seq.choose id)
