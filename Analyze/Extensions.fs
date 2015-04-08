module Extensions

module Seq =
  let split a =
    Seq.map (fun x -> fst x) a, Seq.map (fun x -> snd x) a

  let zip' a =
    Seq.zip (fst a) (snd a)

  let swap a =
    split a |> (fun x -> snd x, fst x) |> zip'

  let apply f g a =
    let s = split a
    zip' (f (fst s), g (snd s))

  let applyR f =
    apply id f

  let applyL f =
    apply f id

  let tuple f g a =
    Seq.map (fun x -> f x, g x) a

  let mapR f =
    tuple (fst >> id) (snd >> f)

  let mapL f =
    tuple (fst >> f) (snd >> id)

  let rec catzip a b =
    if Seq.isEmpty a then
      b
    else
      seq { yield Seq.head a; yield! (catzip b (Seq.skip 1 a)) }

  let rHead a =
    seq { yield Seq.head a; yield! a }

  let rLast a =
    seq { yield! a; yield Seq.last a }

  let show f x =
    eprintfn "%A" (f x); x
