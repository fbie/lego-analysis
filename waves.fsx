module Waves =
  let coeffs = [| 0.4829629131; 0.8365163037; 0.2241438680; -0.1294095226 |]
  let scales = [| coeffs.[3]; -coeffs.[2]; coeffs.[1]; -coeffs.[0] |]

  // See http://fsharpnews.blogspot.dk/2006/12/array-manipulation-continued.html
  let d4 a =
    let rec d4_aux (a: float []) n =
      let n2 = n >>> 1 in
      let tmp = Array.create n 0.
      for i = 0 to (n2 - 2) do
        tmp.[i] <- a.[i*2] * coeffs.[0] + a.[i*2+1] * coeffs.[1] + a.[i*2+2] * coeffs.[2] + a.[i*2+3] * coeffs.[3];
        tmp.[i+n2] <- a.[i*2] * scales.[0] + a.[i*2+1] * scales.[1] + a.[i*2+2] * scales.[2] + a.[i*2+3] * scales.[3];
      tmp.[n2 - 1] <- a.[n-2] * coeffs.[0] + a.[n-1] * coeffs.[1] + a.[0] * coeffs.[2] + a.[1] * coeffs.[3];
      tmp.[2 * n2 - 1] <- a.[n-2] * scales.[0] + a.[n-1] * scales.[1] + a.[0] * scales.[2] + a.[1] * scales.[3];
      Array.blit tmp 0 a 0 n;
      if n > 4 then d4_aux a n2 else a
    d4_aux a (Array.length a)
