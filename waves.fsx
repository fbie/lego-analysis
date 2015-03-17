module Waves =
  let private dot a b =
    Array.map2 (fun x y -> x * y) a b |> Array.sum

  let private window i n (arr: 'a []) =
    [| for j in 0 .. n - 1 do yield arr.[i + j] |]

  let private wrap n (arr: 'a []) =
    let l = Array.length arr - 1
    [| for j in -(n >>> 1) .. (n >>> 1) - 1 do yield if j < 0 then arr.[l + j] else arr.[j] |]

  // See http://fsharpnews.blogspot.dk/2006/12/array-manipulation-continued.html
  let rec private transform h g a n =
    let n2 = n >>> 1 in
    let d = Array.length h
    let tmp = Array.create n 0.
    for i = 0 to (n2 - d / 2) - 1 do
      let win = window (i * 2) d a
      tmp.[i] <- dot win h
      tmp.[i + n2] <- dot win g
    let wrap = wrap d a
    tmp.[n2 - 1] <- dot wrap h
    tmp.[2 * n2 - 1] <- dot wrap g
    Array.blit tmp 0 a 0 n;
    if n > d then transform h g a n2 else a

  let d2 a =
    let h = [| -0.12940952255092145; 0.22414386804185735; 0.836516303737469; 0.48296291314469025 |]
    let g = [| -0.48296291314469025; 0.836516303737469; -0.22414386804185735; -0.12940952255092145 |]
    transform h g a (Array.length a)
