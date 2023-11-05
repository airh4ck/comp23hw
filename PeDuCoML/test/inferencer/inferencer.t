  $ ./inferencer_test.exe <<- EOF
  > let main x y = x + y
  > EOF
  main: int -> int -> int
  $ ./inferencer_test.exe <<- EOF
  > let main (x, y) = x + y
  > EOF
  main: int * int -> int
  $ ./inferencer_test.exe <<- EOF
  > let main ((x, _) :: [('c', "asdf")]) = x
  > EOF
  main: char * string list -> char
  $ ./inferencer_test.exe <<- EOF
  > let add x y = x + y
  > let main = add 1 2 3
  > EOF
  Unification failed: type of the expression is int -> 'e but expected type was int
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> []
  >   | x -> x
  > EOF
  f: 'c list -> 'c list
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> ()
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> ())
  >   | x :: y -> ()
  > EOF
  f: 'f * 'g list -> unit
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> ()
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> ())
  >   | x :: y -> 
  >     match x with 
  >     | (1, 'c') -> ()
  >     | _ -> ()
  > EOF
  f: int * char list -> unit
  $ ./inferencer_test.exe <<- EOF
  > let f arg = match arg with
  >   | [] -> ()
  >   | [x] -> 
  >     (match x with
  >     | (x, y) -> ())
  >   | x :: y -> 
  >     match (x, f y) with 
  >     | (1, 'c') -> ()
  >     | _ -> ()
  > EOF
  No such variable: f
$ ./inferencer_test.exe <<- EOF
> let rec f arg = match arg with
>   | [] -> ()
>   | [x] -> 
>     (match x with
>     | (x, y) -> ())
>   | x :: y -> 
>     match x, (f y) with 
>     | (_, 'c') -> ()
> EOF
Unification failed: (f y) returns unit, but is matched with char
$ ./inferencer_test.exe <<- EOF
> let rec f arg = match arg with
>   | [] -> ()
>   | x :: y -> 
>     match (f y) with
>     | 'c' -> ()
>     | _ -> ()
> EOF
Unification failed: (f y) returns unit, but is matched with char
  $ ./inferencer_test.exe <<- EOF
  > let main = 
  >   let rec even n =
  >     match n with
  >     | 0 -> true
  >     | x -> even (x-1)
  >   and odd n =
  >     match n with
  >     | 0 -> false
  >     | x -> even (x-1)
  >   in even 2
  > EOF
  main: bool
