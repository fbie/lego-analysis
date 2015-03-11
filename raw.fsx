#r "packages/FSharp.Data.2.1.1/lib/net40/FSharp.Data.dll"
open FSharp.Data

#load "time.fsx"
open Time

module Raw =
  open FSharp.Data.CsvExtensions
  open Time

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
    let left  = toEye "L" row
    let right = toEye "R" row
    let raw = { x = row?Rwx.AsFloat(); y = row?Rwy.AsFloat() };
    let avg = { x = row?Avx.AsFloat(); y = row?Avy.AsFloat() };
    {eT = secs (row?eT.AsFloat()); dT = secs (row?dT.AsFloat()); aT = secs (row?aT.AsFloat());
     fixated = row?Fix = "F"; state = row?State;
     rawPos = raw; avgPos = avg;
     leftEye = left; rightEye = right;
     startT = secs (row?startT.AsFloat())}

  let private load (csv: string) =
    CsvFile.Load(csv)

  let parse csv =
    let raw = load csv
    raw.Rows |> Seq.map toRaw
