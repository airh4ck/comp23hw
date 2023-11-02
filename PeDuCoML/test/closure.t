  $ ./closure_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  let main  = fun x -> x
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  let main k = let add_k k x y = (x + y) * k in ((add_k k) 1) 2
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = () in
  >   (42 + add_k 42 (-42))
  > EOF
  let main k = let add_k k x y = (x + y) * k in let waste_of_space  = () in 42 + (((add_k k) 42) (-42))
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  let main k = let add_k k x y = (x + y) * k in -(((add_k k) 1) 2)
  $ ./closure_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  let f ini = let g ini acc = ini :: acc in let h head :: tail = tail in (g ini) (h ([1; 2; 3; 42]))
$ ./closure_test.exe <<- EOF
> let f ini = 
>   let g acc = ini :: acc in
>   let h (head :: tail) = tail in
>   h (h (g []))
> EOF
let f ini = let g ini acc = ini :: acc in let h head :: tail = tail in h (h ((g ini) ([])))
  $ ./closure_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  let fac n = let rec fack n k = if n ≤ 1 then k 1 else (fack (n - 1)) (((fun n k m -> k (m * n)) n) k) in (fack n) (fun x -> x)
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  let gen seed1 seed2 = let gen seed2 seed1 n = (n * seed2) + (seed1 * 42) in [((gen seed2) seed1) 1; ((gen seed2) seed1) 2; ((gen seed2) seed1) 3]
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  let gen seed1 seed2 = let gen seed2 seed1 n = (n * seed2) + (seed1 * 42) in (((gen seed2) seed1) 0) :: ([((gen seed2) seed1) 1; ((gen seed2) seed1) 2; ((gen seed2) seed1) 3])
