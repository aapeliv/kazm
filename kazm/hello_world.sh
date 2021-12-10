set -e

# compile kazm
opam config exec -- ocamlbuild -use-ocamlfind kazm.native
# run the kazm compiler to output LLVM IR
cat tests/codegen.kazm | ./kazm.native > hello_world.ll
# compile LLVM IR into assembly
llc --relocation-model=pic hello_world.ll
# compile builtins
cc -c builtins.c -o builtins.o
# compile hello world
cc -o hello_world builtins.o hello_world.s
# run!
./hello_world
