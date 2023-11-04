  $ ./defunc_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  Ok!
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  Ok!
  $ ./defunc_test.exe <<- EOF
  > let main = 
  >   let const = fun f s -> f in
  >   const
  > EOF
  Ok!
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
  Ok!
