# Kazm

Kazm is a C-like programming language implemented in OCaml for the Programming Languages and Translators class at Columbia University.

In addition to a basic subset of C, Kazm also supports lightweight classes, arrays, and a scoping mechanism that automatically destroys objects.

## Compiling a program

The easiest way to run the Kazm compiler is using Docker and the bundled helper. First clone the repository and enter the `kazm` directory, then compile the included `hello_world.kazm` file in the Kazm Docker container:

```sh
git clone git@github.com:aapeliv/plt.git
cd kazm/
docker run --rm -it -v $(pwd):/home/kazm -w=/home/kazm aapeliv/plt ./compile.py --run hello_world.kazm
```

The helper can be run without the `--run` switch in which case the compiler will simply produce an executable but not run it.

## Running all tests

```sh
cd kazm/
docker run --rm -it -v $(pwd):/home/kazm -w=/home/kazm aapeliv/plt ./test.py
```
