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
  
  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF
