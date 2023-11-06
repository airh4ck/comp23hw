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
  let `ll_0 i0 = i0
  let main = `ll_0
  $ ./anf_test.exe <<- EOF
  > let main = fun _ -> "hello :)"
  > EOF
  let `ll_0 i0 = "hello :)"
  let main = `ll_0
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  let `ll_0 i0 i1 = i0
  let `ll_1 i2 i3 = let i4 =
    `ll_0 i3 in
    i4
  let `ll_2 i5 i6 = i5
  let main i7 = let i9 =
    `ll_2 i7 in
    let i8 =
    `ll_1 i9 in
    i8
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k
  >   in add_k 1 2
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i1 + i2 in
    let i3 =
    i4 * i0 in
    i3
  let main i5 = let i8 =
    `ll_0 i5 in
    let i7 =
    i8 1 in
    let i6 =
    i7 2 in
    i6
  $ ./anf_test.exe <<- EOF
  > let main k = 
  >   let add_k x y = (x + y) * k in
  >   let waste_of_space = () in
  >   (42 + add_k 42 (-42))
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i1 + i2 in
    let i3 =
    i4 * i0 in
    i3
  let main i5 = let i6 =
    () in
    let i11 =
    `ll_0 i5 in
    let i9 =
    i11 42 in
    let i10 =
    -42 in
    let i8 =
    i9 i10 in
    let i7 =
    42 + i8 in
    i7
  $ ./anf_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   g (h [1; 2; 3; 42])
  > EOF
  let `ll_0 i0 i1 = let i2 =
    i0 :: i1 in
    i2
  let `ll_1 i3 = let i5 =
    `head i3 in
    let i4 =
    i5 in
    let i7 =
    `tail i3 in
    let i6 =
    i7 in
    i6
  let f i8 = let i10 =
    `ll_0 i8 in
    let i12 =
    [1; 2; 3; 42] in
    let i11 =
    `ll_1 i12 in
    let i9 =
    i10 i11 in
    i9
  $ ./anf_test.exe <<- EOF
  > let f ini = 
  >   let g acc = ini :: acc in
  >   let h (head :: tail) = tail in
  >   h (h (g []))
  > EOF
  let `ll_0 i0 i1 = let i2 =
    i0 :: i1 in
    i2
  let `ll_1 i3 = let i5 =
    `head i3 in
    let i4 =
    i5 in
    let i7 =
    `tail i3 in
    let i6 =
    i7 in
    i6
  let f i8 = let i12 =
    `ll_0 i8 in
    let i13 =
    [] in
    let i11 =
    i12 i13 in
    let i10 =
    `ll_1 i11 in
    let i9 =
    `ll_1 i10 in
    i9
  $ ./anf_test.exe <<- EOF
  > let fac n =
  >   let rec fack n k =
  >     if n <= 1 then k 1
  >     else fack (n - 1) (fun m -> k (m * n))
  >   in
  >   fack n (fun x -> x)
  > EOF
  let `ll_1 i0 i1 i2 = let i4 =
    i2 * i0 in
    let i3 =
    i1 i4 in
    i3
  let `ll_0 i5 i6 = let i8 =
    i5 ≤ 1 in
    let i9 =
    i6 1 in
    let i14 =
    i5 - 1 in
    let i11 =
    `ll_0 i14 in
    let i13 =
    `ll_1 i5 in
    let i12 =
    i13 i6 in
    let i10 =
    i11 i12 in
    let i7 =
    if i8 then i9 else i10 in
    i7
  let `ll_2 i15 = i15
  let fac i16 = let i18 =
    `ll_0 i16 in
    let i17 =
    i18 `ll_2 in
    i17
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   [gen 1; gen 2; gen 3]
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i2 * i0 in
    let i5 =
    i1 * 42 in
    let i3 =
    i4 + i5 in
    i3
  let gen i6 i7 = let i17 =
    `ll_0 i7 in
    let i16 =
    i17 i6 in
    let i9 =
    i16 1 in
    let i15 =
    `ll_0 i7 in
    let i14 =
    i15 i6 in
    let i10 =
    i14 2 in
    let i13 =
    `ll_0 i7 in
    let i12 =
    i13 i6 in
    let i11 =
    i12 3 in
    let i8 =
    [i9; i10; i11] in
    i8
  $ ./anf_test.exe <<- EOF
  > let gen seed1 seed2 = 
  >   let gen n = n * seed2 + seed1 * 42 in
  >   gen 0 :: [gen 1; gen 2; gen 3]
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i2 * i0 in
    let i5 =
    i1 * 42 in
    let i3 =
    i4 + i5 in
    i3
  let gen i6 i7 = let i21 =
    `ll_0 i7 in
    let i20 =
    i21 i6 in
    let i9 =
    i20 0 in
    let i19 =
    `ll_0 i7 in
    let i18 =
    i19 i6 in
    let i11 =
    i18 1 in
    let i17 =
    `ll_0 i7 in
    let i16 =
    i17 i6 in
    let i12 =
    i16 2 in
    let i15 =
    `ll_0 i7 in
    let i14 =
    i15 i6 in
    let i13 =
    i14 3 in
    let i10 =
    [i11; i12; i13] in
    let i8 =
    i9 :: i10 in
    i8
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> z
  > EOF
  let `ll_0 i0 = let i1 =
    i0 in
    i0
  let main i2 = `ll_0
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   fun z ->
  >   match z with
  >   | y -> y
  > EOF
  let `ll_0 i0 = let i1 =
    i0 in
    i1
  let main i2 = `ll_0
  $ ./anf_test.exe <<-EOF
  > let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
  > let main = factorial 6
  > EOF
  let factorial i0 = let i2 =
    i0 ≤ 1 in
    let i5 =
    i0 - 1 in
    let i4 =
    factorial i5 in
    let i3 =
    i0 * i4 in
    let i1 =
    if i2 then 1 else i3 in
    i1
  let main = let i6 =
    factorial 6 in
    i6
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
  let map i0 i1 = let i14 =
    `length i1 in
    let i13 =
    i14 = 0 in
    let i3 =
    if i13 then false else true in
    let i5 =
    `head i1 in
    let i4 =
    i5 in
    let i7 =
    `tail i1 in
    let i6 =
    i7 in
    let i10 =
    i0 i4 in
    let i12 =
    map i0 in
    let i11 =
    i12 i6 in
    let i8 =
    i10 :: i11 in
    let i9 =
    [] in
    let i2 =
    if i3 then i8 else i9 in
    i2
  let tuple_map i15 i16 = let i25 =
    `at_tuple i16 in
    let i18 =
    i25 1 in
    let i17 =
    i18 in
    let i24 =
    `at_tuple i16 in
    let i20 =
    i24 0 in
    let i19 =
    i20 in
    let i22 =
    i15 i19 in
    let i23 =
    i15 i17 in
    let i21 =
    (i22, i23) in
    i21
  let `ll_0 i26 = let i27 =
    i26 * 2 in
    i27
  let main = let i33 =
    tuple_map `ll_0 in
    let i29 =
    map i33 in
    let i31 =
    (1, 2) in
    let i32 =
    (5, 6) in
    let i30 =
    [i31; i32] in
    let i28 =
    i29 i30 in
    i28
  $ ./anf_test.exe <<- EOF
  > let add_cps x y = fun k -> k (x + y)
  > let square_cps x = fun k -> k (x * x)
  > let pythagoras_cps x y = fun k ->
  >   square_cps x (fun x_squared ->
  >     square_cps y (fun y_squared ->
  >       add_cps x_squared y_squared k))
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i1 + i0 in
    let i3 =
    i2 i4 in
    i3
  let add_cps i5 i6 = let i8 =
    `ll_0 i6 in
    let i7 =
    i8 i5 in
    i7
  let `ll_1 i9 i10 = let i12 =
    i9 * i9 in
    let i11 =
    i10 i12 in
    i11
  let square_cps i13 = let i14 =
    `ll_1 i13 in
    i14
  let `ll_4 i15 i16 i17 = let i20 =
    add_cps i15 in
    let i19 =
    i20 i17 in
    let i18 =
    i19 i16 in
    i18
  let `ll_3 i21 i22 i23 = let i25 =
    square_cps i21 in
    let i27 =
    `ll_4 i23 in
    let i26 =
    i27 i22 in
    let i24 =
    i25 i26 in
    i24
  let `ll_2 i28 i29 i30 = let i32 =
    square_cps i29 in
    let i34 =
    `ll_3 i28 in
    let i33 =
    i34 i30 in
    let i31 =
    i32 i33 in
    i31
  let pythagoras_cps i35 i36 = let i38 =
    `ll_2 i36 in
    let i37 =
    i38 i35 in
    i37
  $ ./anf_test.exe <<- EOF
  > let thrice_cps f_cps x = fun k ->
  >   f_cps x (fun fx ->
  >     f_cps fx (fun ffx ->
  >       f_cps ffx k))
  > EOF
  let `ll_2 i0 i1 i2 = let i4 =
    i1 i2 in
    let i3 =
    i4 i0 in
    i3
  let `ll_1 i5 i6 i7 = let i9 =
    i6 i7 in
    let i11 =
    `ll_2 i5 in
    let i10 =
    i11 i6 in
    let i8 =
    i9 i10 in
    i8
  let `ll_0 i12 i13 i14 = let i16 =
    i13 i12 in
    let i18 =
    `ll_1 i14 in
    let i17 =
    i18 i13 in
    let i15 =
    i16 i17 in
    i15
  let thrice_cps i19 i20 = let i22 =
    `ll_0 i20 in
    let i21 =
    i22 i19 in
    i21
  $ ./anf_test.exe <<- EOF
  > let main k i = 
  >   match (fun x y -> x + k) with
  >   | f ->
  >     let id = fun x -> x in
  >     f i
  > EOF
  let `ll_0 i0 i1 i2 = let i3 =
    i1 + i0 in
    i3
  let `ll_1 i4 = i4
  let main i5 i6 = let i8 =
    `ll_0 i5 in
    let i7 =
    i8 in
    let i9 =
    i7 i6 in
    i9
  $ ./anf_test.exe debug <<- EOF
  > let main = fun x -> x
  > EOF
  let `ll_0 i0 = i0
  let main = `ll_0
  $ ./anf_test.exe <<- EOF
  > let phi n = 
  >   let rec helper last1 last2 n = 
  >     if n > 0 then helper last2 (last1 + last2) (n - 1) 
  >     else last2 
  >   in helper 1 1 (n - 2)
  > 
  > let main = phi 10
  > EOF
  let `ll_0 i0 i1 i2 = let i4 =
    i2 > 0 in
    let i8 =
    `ll_0 i1 in
    let i9 =
    i0 + i1 in
    let i6 =
    i8 i9 in
    let i7 =
    i2 - 1 in
    let i5 =
    i6 i7 in
    let i3 =
    if i4 then i5 else i1 in
    i3
  let phi i10 = let i14 =
    `ll_0 1 in
    let i12 =
    i14 1 in
    let i13 =
    i10 - 2 in
    let i11 =
    i12 i13 in
    i11
  let main = let i15 =
    phi 10 in
    i15
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
  let `ll_0 i0 i1 = let i12 =
    `length i0 in
    let i11 =
    i12 = 0 in
    let i3 =
    if i11 then false else true in
    let i5 =
    `head i0 in
    let i4 =
    i5 in
    let i7 =
    `tail i0 in
    let i6 =
    i7 in
    let i9 =
    `ll_0 i6 in
    let i10 =
    i1 * i4 in
    let i8 =
    i9 i10 in
    let i2 =
    if i3 then i8 else i1 in
    i2
  let product i13 = let i15 =
    `ll_0 i13 in
    let i14 =
    i15 1 in
    i14
  let main = let i17 =
    [1; 2; 7; 12; 10; 3; 21] in
    let i16 =
    product i17 in
    i16
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
  let `ll_0 i0 i1 = let i12 =
    `length i0 in
    let i11 =
    i12 = 0 in
    let i3 =
    if i11 then false else true in
    let i5 =
    `head i0 in
    let i4 =
    i5 in
    let i7 =
    `tail i0 in
    let i6 =
    i7 in
    let i9 =
    `ll_0 i6 in
    let i10 =
    i1 + i4 in
    let i8 =
    i9 i10 in
    let i2 =
    if i3 then i8 else i1 in
    i2
  let sum i13 = let i15 =
    `ll_0 i13 in
    let i14 =
    i15 0 in
    i14
  let main = let i17 =
    [1; 2; 7; 12; 10; 3; 21; 101; 78; 42; 38] in
    let i16 =
    sum i17 in
    i16
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
  let `ll_0 i0 i1 = let i10 =
    `length i0 in
    let i9 =
    i10 = 0 in
    let i3 =
    if i9 then false else true in
    let i5 =
    `tail i0 in
    let i4 =
    i5 in
    let i7 =
    `ll_0 i4 in
    let i8 =
    i1 + 1 in
    let i6 =
    i7 i8 in
    let i2 =
    if i3 then i6 else i1 in
    i2
  let length i11 = let i13 =
    `ll_0 i11 in
    let i12 =
    i13 0 in
    i12
  let main = let i15 =
    [1; 23; 12; 657; 123; 346; 6; 234; 99; 34; 78; 28; 123; 0] in
    let i14 =
    length i15 in
    i14
  $ ./anf_test.exe <<- EOF
  > let rec filter predicate list =
  >   match list with
  >     | h :: t -> if predicate h then h :: filter predicate t else filter predicate t
  >     | _ -> []
  > 
  > let main = filter (fun v -> v > 10) [1;2;3]
  > EOF
  let filter i0 i1 = let i17 =
    `length i1 in
    let i16 =
    i17 = 0 in
    let i3 =
    if i16 then false else true in
    let i5 =
    `head i1 in
    let i4 =
    i5 in
    let i7 =
    `tail i1 in
    let i6 =
    i7 in
    let i10 =
    i0 i4 in
    let i15 =
    filter i0 in
    let i14 =
    i15 i6 in
    let i11 =
    i4 :: i14 in
    let i13 =
    filter i0 in
    let i12 =
    i13 i6 in
    let i8 =
    if i10 then i11 else i12 in
    let i9 =
    [] in
    let i2 =
    if i3 then i8 else i9 in
    i2
  let `ll_0 i18 = let i19 =
    i18 > 10 in
    i19
  let main = let i21 =
    filter `ll_0 in
    let i22 =
    [1; 2; 3] in
    let i20 =
    i21 i22 in
    i20
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
  let `ll_0 i0 = let i1 =
    i0 * i0 in
    i1
  let count_solutions_of_sq_equation i2 i3 i4 = let i11 =
    `ll_0 i3 in
    let i13 =
    4 * i2 in
    let i12 =
    i13 * i4 in
    let i6 =
    i11 - i12 in
    let i5 =
    i6 in
    let i8 =
    i5 > 0 in
    let i10 =
    i5 = 0 in
    let i9 =
    if i10 then 1 else 0 in
    let i7 =
    if i8 then 2 else i9 in
    i7
  let main = let i16 =
    count_solutions_of_sq_equation 2 in
    let i15 =
    i16 9 in
    let i14 =
    i15 4 in
    i14
  $ ./anf_test.exe <<- EOF
  > let f x y z =
  >   match x, y, z with
  >     | true, true, true -> true
  >     | false, false, false -> true
  >     | _ -> false
  > 
  > let main = f (10 * 5 > 49) (58 / 2 = 27) (10 <> 20)
  > EOF
  let f i0 i1 i2 = let i32 =
    (i0, i1, i2) in
    let i31 =
    `at_tuple i32 in
    let i30 =
    i31 0 in
    let i25 =
    i30 = true in
    let i29 =
    (i0, i1, i2) in
    let i28 =
    `at_tuple i29 in
    let i27 =
    i28 1 in
    let i26 =
    i27 = true in
    let i20 =
    i25 && i26 in
    let i24 =
    (i0, i1, i2) in
    let i23 =
    `at_tuple i24 in
    let i22 =
    i23 2 in
    let i21 =
    i22 = true in
    let i4 =
    i20 && i21 in
    let i19 =
    (i0, i1, i2) in
    let i18 =
    `at_tuple i19 in
    let i17 =
    i18 0 in
    let i12 =
    i17 = false in
    let i16 =
    (i0, i1, i2) in
    let i15 =
    `at_tuple i16 in
    let i14 =
    i15 1 in
    let i13 =
    i14 = false in
    let i7 =
    i12 && i13 in
    let i11 =
    (i0, i1, i2) in
    let i10 =
    `at_tuple i11 in
    let i9 =
    i10 2 in
    let i8 =
    i9 = false in
    let i6 =
    i7 && i8 in
    let i5 =
    if i6 then true else false in
    let i3 =
    if i4 then true else i5 in
    i3
  let main = let i40 =
    10 * 5 in
    let i39 =
    i40 > 49 in
    let i36 =
    f i39 in
    let i38 =
    58 / 2 in
    let i37 =
    i38 = 27 in
    let i34 =
    i36 i37 in
    let i35 =
    10 <> 20 in
    let i33 =
    i34 i35 in
    i33
  $ ./anf_test.exe <<- EOF
  > let pifagor_check = fun x y z -> x * x + y * y = z * z
  > 
  > let main = pifagor_check 3 4 5
  > EOF
  let `ll_0 i0 i1 i2 = let i6 =
    i0 * i0 in
    let i7 =
    i1 * i1 in
    let i4 =
    i6 + i7 in
    let i5 =
    i2 * i2 in
    let i3 =
    i4 = i5 in
    i3
  let pifagor_check = `ll_0
  let main = let i10 =
    pifagor_check 3 in
    let i9 =
    i10 4 in
    let i8 =
    i9 5 in
    i8
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
  let `ll_0 i0 i1 = let i40 =
    (i0, i1) in
    let i39 =
    `at_tuple i40 in
    let i38 =
    i39 0 in
    let i37 =
    `length i38 in
    let i36 =
    i37 = 0 in
    let i29 =
    if i36 then false else true in
    let i35 =
    (i0, i1) in
    let i34 =
    `at_tuple i35 in
    let i33 =
    i34 1 in
    let i32 =
    `length i33 in
    let i31 =
    i32 = 0 in
    let i30 =
    if i31 then false else true in
    let i3 =
    i29 && i30 in
    let i28 =
    (i0, i1) in
    let i27 =
    `at_tuple i28 in
    let i26 =
    i27 1 in
    let i5 =
    `head i26 in
    let i4 =
    i5 in
    let i25 =
    (i0, i1) in
    let i24 =
    `at_tuple i25 in
    let i23 =
    i24 1 in
    let i7 =
    `tail i23 in
    let i6 =
    i7 in
    let i22 =
    (i0, i1) in
    let i21 =
    `at_tuple i22 in
    let i20 =
    i21 0 in
    let i9 =
    `head i20 in
    let i8 =
    i9 in
    let i19 =
    (i0, i1) in
    let i18 =
    `at_tuple i19 in
    let i17 =
    i18 0 in
    let i11 =
    `tail i17 in
    let i10 =
    i11 in
    let i14 =
    i8 + i4 in
    let i16 =
    `ll_0 i10 in
    let i15 =
    i16 i6 in
    let i12 =
    i14 :: i15 in
    let i13 =
    [] in
    let i2 =
    if i3 then i12 else i13 in
    i2
  let matrix_sum i41 i42 = let i82 =
    (i41, i42) in
    let i81 =
    `at_tuple i82 in
    let i80 =
    i81 0 in
    let i79 =
    `length i80 in
    let i78 =
    i79 = 0 in
    let i71 =
    if i78 then false else true in
    let i77 =
    (i41, i42) in
    let i76 =
    `at_tuple i77 in
    let i75 =
    i76 1 in
    let i74 =
    `length i75 in
    let i73 =
    i74 = 0 in
    let i72 =
    if i73 then false else true in
    let i44 =
    i71 && i72 in
    let i70 =
    (i41, i42) in
    let i69 =
    `at_tuple i70 in
    let i68 =
    i69 1 in
    let i46 =
    `head i68 in
    let i45 =
    i46 in
    let i67 =
    (i41, i42) in
    let i66 =
    `at_tuple i67 in
    let i65 =
    i66 1 in
    let i48 =
    `tail i65 in
    let i47 =
    i48 in
    let i64 =
    (i41, i42) in
    let i63 =
    `at_tuple i64 in
    let i62 =
    i63 0 in
    let i50 =
    `head i62 in
    let i49 =
    i50 in
    let i61 =
    (i41, i42) in
    let i60 =
    `at_tuple i61 in
    let i59 =
    i60 0 in
    let i52 =
    `tail i59 in
    let i51 =
    i52 in
    let i58 =
    `ll_0 i49 in
    let i55 =
    i58 i45 in
    let i57 =
    matrix_sum i51 in
    let i56 =
    i57 i47 in
    let i53 =
    i55 :: i56 in
    let i54 =
    [] in
    let i43 =
    if i44 then i53 else i54 in
    i43
  let matrix1 = let i84 =
    [1; 5; 7] in
    let i85 =
    [13; 32; 56] in
    let i86 =
    [45; 2; 17] in
    let i83 =
    [i84; i85; i86] in
    i83
  let matrix2 = let i88 =
    [4; 29; 0] in
    let i89 =
    [79; 12; 66] in
    let i90 =
    [8; 88; 19] in
    let i87 =
    [i88; i89; i90] in
    i87
  let main = let i92 =
    matrix_sum matrix1 in
    let i91 =
    i92 matrix2 in
    i91
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
  let `ll_0 i0 i1 = let i14 =
    `length i1 in
    let i13 =
    i14 = 0 in
    let i3 =
    if i13 then false else true in
    let i5 =
    `head i1 in
    let i4 =
    i5 in
    let i7 =
    `tail i1 in
    let i6 =
    i7 in
    let i10 =
    i4 * i0 in
    let i12 =
    `ll_0 i0 in
    let i11 =
    i12 i6 in
    let i8 =
    i10 :: i11 in
    let i9 =
    [] in
    let i2 =
    if i3 then i8 else i9 in
    i2
  let matrix_mult_number i15 i16 = let i30 =
    `length i15 in
    let i29 =
    i30 = 0 in
    let i18 =
    if i29 then false else true in
    let i20 =
    `head i15 in
    let i19 =
    i20 in
    let i22 =
    `tail i15 in
    let i21 =
    i22 in
    let i28 =
    `ll_0 i16 in
    let i25 =
    i28 i19 in
    let i27 =
    matrix_mult_number i21 in
    let i26 =
    i27 i16 in
    let i23 =
    i25 :: i26 in
    let i24 =
    [] in
    let i17 =
    if i18 then i23 else i24 in
    i17
  let matrix = let i32 =
    [1; 5; 7] in
    let i33 =
    [13; 32; 56] in
    let i34 =
    [45; 2; 17] in
    let i31 =
    [i32; i33; i34] in
    i31
  let main = let i36 =
    matrix_mult_number matrix in
    let i35 =
    i36 5 in
    i35
