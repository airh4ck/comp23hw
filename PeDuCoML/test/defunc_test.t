  $ ./defunc_test.exe <<- EOF
  > let main = fun x -> x
  > EOF
  let `ll_0 x = x
  let main  = `ll_0
  $ ./defunc_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF

  $ ./defunc_test.exe <<- EOF
  > let main = 
  >   let const = fun f s -> f in
  >   const
  > EOF
