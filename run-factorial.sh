cabal run exe:GDFunc -- --emit-c examples/quicksort_linear.gdfunc &&
gcc examples/factorial.c -o examples/factorial &&
./examples/factorial 