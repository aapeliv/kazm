# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "builtins" library designed
# to test linking external code

.PHONY : all
all : kazm.native builtins.o

# "make kazm.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc


# Dependencies for ocamlbuild 

kazm.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind kazm.native

# "make clean" removes all generated files
# Added *.o 

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff 


# Testing the "printbig" example

builtins : builtins.c
	cc -o builtins -DBUILD_TEST builtins.c

# Building the tarball



