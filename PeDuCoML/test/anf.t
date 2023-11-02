  $ ./anf_test.exe <<- EOF
  > let main = 1
  > EOF
  let i0 = 1 in
  i0
  ;;
  $ ./anf_test.exe <<- EOF
  > let main = "asdf"
  > EOF
  let i0 = "asdf" in
  i0
  ;;
  $ ./anf_test.exe <<- EOF
  > let main = 'c'
  > EOF
  let i0 = 'c' in
  i0
  ;;
  $ ./anf_test.exe <<- EOF
  > let main = true
  > EOF
  let i0 = true in
  i0
  ;;
  $ ./anf_test.exe <<- EOF
  > let main = ()
  > EOF
  let i0 = () in
  i0
  ;;
  $ ./anf_test.exe <<- EOF
  > let main = fun x -> x
  > EOF

  $ ./anf_test.exe <<- EOF
  > let main x = 
  >   let const f = fun s -> f in
  >   let rev_const f s = const s in
  >   rev_const (fun _ -> x)
  > EOF