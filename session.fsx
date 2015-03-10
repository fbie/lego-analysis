#load "action.fsx"
open Action
open Time

#r "packages/FnuPlot.0.1.1-beta/lib/net40/FnuPlot.dll"
open FnuPlot
open System
open System.Drawing

module Session =
    let print prefix x =
      printfn "%s=%A" prefix x

    let rec duration =
      function
        | [] -> 0.0<Time.s>
        | (ts: Time.Stamp, _) :: [] -> ts.elapsed
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
        | (ts: Time.Stamp, Action.Next (s, ss)) :: t -> (float ts.elapsed, toFloat s ss) :: progress t
        | (ts, Action.Previous (s, ss)) :: t -> (float ts.elapsed, toFloat s ss ) :: progress t
        | [] -> []
        | _ :: t -> progress t

    let rec zoom =
      function
        | (ts: Time.Stamp, Action.Zoom _) :: t -> ts.elapsed :: zoom t
        | _ :: t -> zoom t
        | [] -> []

    let rec rotate =
      function
        | (ts: Time.Stamp, Action.Rotation _) :: t -> ts.elapsed :: rotate t
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

    let attention actions =
      let rec intervals last actions =
        match actions with
          | (ts: Time.Stamp, Action.Tracking b) :: t -> match b with
                                                          | true -> intervals ts.elapsed t
                                                          | false -> seq {for p in int last .. int ts.elapsed -> float p, 10.0} :: intervals 0.0<Time.s> t
          | (ts, a) :: [] -> if last <> 0.0<Time.s> then seq {for p in int last .. int ts.elapsed -> float p, 10.0} :: [] else []
          | _ :: t -> intervals last t
          | [] -> []
      actions
      |> intervals 0.0<Time.s>
      |> List.fold (fun s t -> s @ (t |> Seq.toList)) []

let rec getArgs =
  function
    | "--" :: t -> t
    | [] -> []
    | _ :: t -> getArgs t

let argv = fsi.CommandLineArgs |> Array.toList |> getArgs
let entries = argv.Head |> Action.parseFile

let gp = new GnuPlot()
gp.Set(style = Style(fill = Pattern 100))
gp.Set(output = Output(Png "plot"))
[ Series.Points (title="attention", data=(entries |> Session.attention))
  Series.Impulses (title="zoom", weight=3, data=(entries
                                                 |> Session.zoom
                                                 |> Session.hist5s))
  Series.Impulses (title="rotate", weight=3, data=(entries
                                                   |> Session.rotate
                                                   |> Session.hist5s
                                                   |> Session.offset 2.5))
  Series.Lines (title="progress", weight=2, data=(entries |> Session.progress))
  ]
|> gp.Plot
