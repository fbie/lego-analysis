module Extensions

module Seq =
  let split a =
    Seq.map (fun x -> fst x) a, Seq.map (fun x -> snd x) a

  let apply f g h i a =
    Seq.zip (a |> Seq.map (fun x -> f x) |> g) (a |> Seq.map (fun x -> h x) |> i)

  let applyR f =
    apply fst id snd f

  let applyL f =
    apply fst f snd id

  let swap a =
    apply snd id fst id a

  let tuple f g =
    Seq.map (fun x -> f x , g x)

  let tupleR f =
    tuple (fst >> id) (snd >> f)

  let tupleL f =
    tuple (fst >> f) (snd >> id)

  let rec zap a b =
    if Seq.isEmpty a then
      b
    else
      seq { yield Seq.head a; yield! (zap b (Seq.skip 1 a)) }
