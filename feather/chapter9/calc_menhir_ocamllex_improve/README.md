# calc_menhir_ocamllex_improved

# dependencies
```
ocamlc utop dune menhir ocamllex make
```
require these tools to build the calculator


# how to use
```
make run
```
execuate the main, namely calculator which support int float type, plus minus times and div operation between two operands and parentheses.

examples:
```
8 + 5.2
7*(1 + 4 /3)
1--2 or 1-(-2)
```

# test
```
make test
```
run Ounit tests for functionality of the calculator.

# utop
```
make 
or
make utop
```

then, you could use `parse` and `interp` to look up the AST and eval output
# clean
```
make clean
```
delete files except source files