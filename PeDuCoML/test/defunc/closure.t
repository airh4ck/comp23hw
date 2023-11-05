  $ ./closure_test.exe debug <<- EOF
  > let main = fun x -> x
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = () in
  >   (42 + add_k 42 (-42))
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  Ok!
$ ./closure_test.exe <<- EOF
> let f ini = 
>   let g acc = ini :: acc in
>   let h (head :: tail) = tail in
>   h (h (g []))
> EOF
Ok! 
  $ ./closure_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> x
  > EOF
  Ok!
  $ ./closure_test.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  Ok!
  $ ./closure_test.exe <<-EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let tuple_map f tuple = match tuple with
  >   | (x, y) -> (f x, f y)
  > 
  > let main = map (tuple_map (fun x -> x * 2)) [(1, 2); (5, 6)]
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  Ok!
  $ ./closure_test.exe <<- EOF
  > let main k i = 
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  Ok!
