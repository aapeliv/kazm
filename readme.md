# PLT class group project

## Running all tests

```sh
cd kazm/
docker run --rm -it -v $(pwd):/home/kazm -w=/home/kazm aapeliv/plt ./test.py
```


## Running the parser

```sh
cd kazm/
ocamlbuild kazm.native
cat sample.kazm | ./kazm.native
```


## Running the array example

In the OCaml/LLVM docker container:

```sh
cd kazm
docker run --rm -it -v $(pwd):/home/kazm -w=/home/kazm aapeliv/plt
# now inside the container
opam config exec -- ocamlbuild -use-ocamlfind kazm.native
# run the kazm compiler to output LLVM IR
cat tests/test-arr.kazm | ./kazm.native > hello_world.ll
# compile LLVM IR into assembly
llc --relocation-model=pic hello_world.ll
# compile builtins
cc -c builtins.c -o builtins.o
# compile hello world
cc -o hello_world builtins.o hello_world.s
# run!
./hello_world
```

## Menhir
```sh
cd kazm
menhir --interpret --interpret-show-cst parser.mly
# void main() {} corresponds to 
VOID IDENTIFIER PAREN_L PAREN_R BRACE_L BRACE_R
# fill in the space between BRACE_L and BRACE_R to test other programs
```