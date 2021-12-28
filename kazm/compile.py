#!/bin/python3

from pathlib import Path
import shutil
import subprocess
import argparse

parser = argparse.ArgumentParser(description="Kazm compiler")
parser.add_argument("file", help="the .kazm file to compile")
parser.add_argument("--run", dest="run", action="store_true", help="Run the compiled file")
parser.set_defaults(run=False, select="")
args = parser.parse_args()

# clean build dir
tmp_dir = Path(".") / "_kazm_build"
shutil.rmtree(tmp_dir, ignore_errors=True)
tmp_dir.mkdir()

def pipe_through(args, input_):
    out = subprocess.run(args, input=input_, capture_output=True)
    return out.returncode, out.stdout, out.stderr

def run(args):
    out = subprocess.run(args, capture_output=True)
    return out.returncode, out.stdout.decode("utf8"), out.stderr.decode("utf8")

print(f"Building kazm...")
print(f" Compiling kazm.native")
code, stdout, stderr = run(["opam", "config", "exec", "--", "ocamlbuild", "-use-ocamlfind", "kazm.native"])
if code != 0:
    print("Failed to compile kazm.native")
    print(stderr)
    print(stdout)
    exit(1)
print(f" Compiling builtins")
builtins_o = f"{tmp_dir}/builtins.o"
code, _, _ = run(["cc", "-c", "builtins.c", "-o", builtins_o])
assert code == 0, "Failed to compile builtins"

filename = args.file

assert filename.endswith(".kazm"), "File must end with .kazm"
name = filename[:-len(".kazm")]

print(f"Compiling '{name}'")
# LLVM IR file
ll_file = f"{tmp_dir}/{name}.ll"
# assembly file
s_file = f"{tmp_dir}/{name}.s"
# executable
ex_file = f"./{name}"
print(f" Compiling '{name}' through kazm...")
with open(filename, "rb") as f:
    code, data, err = pipe_through(["./kazm.native"], f.read())
    # failed to compile
if code != 0:
    exit("Failed to compile with kazm:\n\n" + err.decode("utf8"))
with open(ll_file, "wb") as o:
    o.write(data)
print(f" Compiling '{name}' through LLVM...")
code, _, stderr = run(["llc", "--relocation-model=pic", ll_file, f"-o={s_file}"])
# failed to compile llvm
if code != 0:
    exit("Failed to compile with llvm:\n\n" + stderr)
print(f" Linking '{name}' through cc...")
code, _, stderr = run(["cc", "-o", ex_file, builtins_o, s_file])
if code != 0:
    exit("Failed to link:\n\n" + stderr)
print(f"OK.")
print(f"Compiled code written to '{ex_file}'")

if args.run:
    print(f" Running '{name}'...")
    code, stdout, stderr = run([ex_file])
    if code == 0:
        print()
        print(stdout)
    else:
        exit("Failed to run:\n\n" + stderr)
