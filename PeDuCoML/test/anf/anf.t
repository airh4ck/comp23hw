  $ ./anf_test.exe <<- EOF
  > let main = 1
  > EOF
  let main = 1
  $ ./anf_test.exe <<- EOF
  > let main = "asdf"
  > EOF
  let main = "asdf"
  $ ./anf_test.exe <<- EOF
  > let main = 'c'
  > EOF
  let main = 'c'
  $ ./anf_test.exe <<- EOF
  > let main = true
  > EOF
  let main = true
  $ ./anf_test.exe <<- EOF
  > let main = ()
  > EOF
  let main = ()
  $ ./anf_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  let `ll_0 i1 = let i2 =
    i1 in
    i2
  let main = i0
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF

  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
    
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = () in
  >   (42 + add_k 42 (-42))
  > EOF
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> x
  > EOF
  Ok!
  $ ./anf_test.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  Ok!
  $ ./anf_test.exe <<-EOF
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
  $ ./anf_test.exe <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main k i = 
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  Ok!
  $ ./anf_test.exe debug <<- EOF
  > let main = fun x -> x
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = () in
  >   (42 + add_k 42 (-42))
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   - (add_k 1 2)
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   h (h (g []))
  > EOF
  Ok! 
  $ ./anf_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  Ok!
  $ ./anf_test.exe <<- EOF
  > let phi n = 
  >   let rec helper last1 last2 n = 
  >     if n > 0 then helper last2 (last1 + last2) (n - 1) 
  >     else last2 
  >   in helper 1 1 (n - 2)
  > 
  > let main = phi 10
  > EOF
  
  $ ./anf_test.exe <<- EOF
  > let product list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc * head)
  >     | _ -> acc
  >   in
  >   helper list 1
  > 
  > let main = product [1; 2; 7; 12; 10; 3; 21]
  > EOF
    
  $ ./anf_test.exe <<- EOF
  > let sum list =
  >   let rec helper list acc = match list with
  >     | head :: tail -> helper tail (acc + head)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = sum [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38]
  > EOF
  $ ./anf_test.exe <<- EOF
  > let length list =
  >   let rec helper list acc = match list with
  >     | _ :: tail -> helper tail (acc + 1)
  >     | _ -> acc
  >   in
  >   helper list 0
  > 
  > let main = length [1; 23; 12; 657; 123; 346; 6; 234 ; 99; 34; 78; 28; 123; 0]
  > EOF
  14
  $ ./anf_test.exe <<- EOF
  > let head list =
  >   match list with
  >     | h :: _ -> Some h
  >     | _ -> None
  > 
  > let main = head [97; 81; 0; 54; 13]
  > EOF
  Some 97
  $ ./anf_test.exe <<- EOF
  > let head list =
  >   match list with
  >     | h :: _ -> Some h
  >     | _ -> None
  > 
  > let main = head []
  > EOF
  None 
  $ ./anf_test.exe <<- EOF
  > let tail list =
  >   match list with
  >     | _ :: t -> Some t
  >     | _ -> None
  > 
  > let main = tail [97; 81; 0; 54; 13]
  > EOF
  Some [81; 0; 54; 13]
  $ ./anf_test.exe <<- EOF
  > let tail list =
  >   match list with
  >     | _ :: t -> Some t
  >     | _ -> None
  > 
  > let main = tail []
  > EOF
  None 
  $ ./anf_test.exe <<- EOF
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63]
  > EOF
  [12; 3; 0; 1; 2]
  $ ./anf_test.exe <<- EOF
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v > 10) [1;2;3]
  > EOF
  []
  $ ./anf_test.exe <<- EOF
  > let filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v * v < 150) [12; 3; 54; 85; 36; 0; 91; 100; 1; 2; 13; 28; 63]
  > EOF
  No such variable: filter
  $ ./anf_test.exe <<- EOF
  > let main = f 1 2
  > EOF
  No such variable: f
  $ ./anf_test.exe <<- EOF
  > let count_solutions_of_sq_equation a b c =
  >   let sq x = x * x
  >   in
  >   let d = sq b - 4 * a * c
  >   in
  >   if d > 0 then 2 else (if d = 0 then 1 else 0)
  > 
  > let main = count_solutions_of_sq_equation 2 9 4
  > EOF
  
  $ ./anf_test.exe <<- EOF
  > let rec map f list = match list with
  >   | head :: tail -> f head :: map f tail
  >   | _ -> []
  > 
  > let sq = fun x -> x * x
  > 
  > let main = map sq [1;2;3;4;5;6;7;8;9;10]
  > EOF
  [1; 4; 9; 16; 25; 36; 49; 64; 81; 100]
  $ ./anf_test.exe <<- EOF
  > let f x y z =
  >   match x, y, z with
  >     | true, true, true -> true
  >     | false, false, false -> true
  >     | _ -> false
  > 
  > let main = f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20)
  > EOF
  false
  $ ./anf_test.exe <<- EOF
  > let main = "abc" + "def"
  > EOF
  Unification failed: type of the expression is string but expected type was int
  $ ./anf_test.exe <<- EOF
  > let pifagor_check = fun x y z -> x * x + y * y = z * z
  > 
  > let main = pifagor_check 3 4 5
  > EOF
  true
  $ ./anf_test.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > 
  > let main = check_password "qwerty"
  > EOF
  Error "FAIL"
  $ ./anf_test.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > 
  > let main = check_password "qwerty123"
  > EOF
  Ok "success"
  $ ./anf_test.exe <<- EOF
  > let check_password password = 
  >   match password with
  >     | "qwerty123" -> Ok "success"
  >     | _ -> Error "FAIL"
  > EOF
  <fun>
  $ ./anf_test.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 2
  > 
  > let main = (if idx = 1 then fst else snd) (13, 225)
  > EOF
  225
  $ ./anf_test.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 1
  > 
  > let main = (if idx = 1 then fst else snd) (13, 225)
  > EOF
  13
  $ ./anf_test.exe <<- EOF
  > let fst pair =
  >   match pair with (x, _) -> x
  > 
  > let snd pair =
  >   match pair with (_, y) -> y
  > 
  > let idx = 1
  > 
  > let main = (if idx = 1 then fst else snd) (13, 45, 89)
  > EOF
  Unification failed: type of the expression is int * int * int but expected type was 'p * 'p
  $ ./anf_test.exe <<- EOF
  > let rec matrix_sum m1 m2 =
  >   let rec lines_sum l1 l2 =
  >     match l1, l2 with
  >       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
  >       | _, _ -> []
  >   in
  >   match m1, m2 with
  >     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
  >     | _, _ -> []
  > 
  > let matrix1 = [[1;  5;  7 ];
  >                [13; 32; 56];
  >                [45; 2;  17]]
  > 
  > let matrix2 = [[4;  29;  0];
  >                [79; 12; 66];
  >                [8;  88; 19]]
  > 
  > let main = matrix_sum matrix1 matrix2
  > EOF
  [[5; 34; 7]; [92; 44; 122]; [53; 90; 36]]
  $ ./anf_test.exe <<- EOF
  > let rec matrix_mult_number matrix number =
  >   let rec line_mult_number line =
  >     match line with
  >       | head :: tail -> (head * number) :: line_mult_number tail
  >       | _ -> []
  >   in
  >   match matrix with
  >     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
  >     | _ -> []
  > 
  > let matrix = [[1;  5;  7 ];
  >               [13; 32; 56];
  >               [45; 2;  17]]
  > 
  > let main = matrix_mult_number matrix 5
  > EOF
  [[5; 25; 35]; [65; 160; 280]; [225; 10; 85]]
  $ ./anf_test.exe <<- EOF
  > let rec matrix_mult_number matrix number =
  >   let rec line_mult_number line =
  >     match line with
  >       | head :: tail -> (head * number) :: line_mult_number tail
  >       | _ -> []
  >   in
  >   match matrix with
  >     | head :: tail -> line_mult_number head :: matrix_mult_number tail number
  >     | _ -> []
  > 
  > let rec matrix_sum m1 m2 =
  >   let rec lines_sum l1 l2 =
  >     match l1, l2 with
  >       | h1 :: t1, h2 :: t2 -> (h1 + h2) :: lines_sum t1 t2
  >       | _, _ -> []
  >   in
  >   match m1, m2 with
  >     | h1 :: t1, h2 :: t2 -> lines_sum h1 h2 :: matrix_sum t1 t2
  >     | _, _ -> []
  > 
  > let matrix1 = [[1;  5;  7 ];
  >                [13; 32; 56];
  >                [45; 2;  17]]
  > 
  > let matrix2 = [[4;  29;  0];
  >                [79; 12; 66];
  >                [8;  88; 19]]
  > 
  > let main = matrix_sum (matrix_mult_number matrix1 2) (matrix_mult_number matrix2 7)
  > EOF
  [[30; 213; 14]; [579; 148; 574]; [146; 620; 167]]
  $ ./anf_test.exe <<- EOF
  > let int_list = [1; 2; 3]
  > 
  > let main = "0" :: int_list
  > EOF