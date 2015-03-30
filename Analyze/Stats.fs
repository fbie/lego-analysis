module Analyze.Stats

let std f a =
  if not (Seq.isEmpty a) then
    let avg = Seq.averageBy f a
    let err = (Seq.fold (fun s x -> s + (f x - avg) ** 2.0) 0.0 a |> sqrt) / (Seq.length >> float) a
    avg, err
  else
    0.0, 0.0

let sstd f a =
  if Seq.length a > 1 then
    let avg = Seq.averageBy f a
    let err = (Seq.fold (fun s x -> s + (f x - avg) ** 2.0) 0.0 a |> sqrt) / ((Seq.length >> float) a - 1.0)
    avg, err
  else
    0.0, 0.0

let sem f a =
  let n = Seq.length a
  if n > 0 then
    let (avg, err) = sstd f a
    avg, err / float n
  else
    0.0, 0.0

let weighted w a =
  (Seq.map2 (fun x y -> x * y) a w |> Seq.sum) / Seq.sum w
