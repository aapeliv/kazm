# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the utils library

.PHONY : all
all : kazm.native  builtins.o

# "make kazm.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

kazm.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind kazm.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff test* fail*  

# Can use this to test utils.c later

builtins : builtins.c
	cc -o builtins builtins.c
	#-DBUILD_TEST