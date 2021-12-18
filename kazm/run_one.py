#!/usr/bin/python
import difflib
from glob import glob
from pathlib import Path
import shutil
import subprocess
import argparse
import sys 

if (len(sys.argv) != 2 or sys.argv[1].endswith(".kazm") == False):
    print("Usage: ./run_one.py [*.kazm]")
    exit(1)

my_file = sys.argv[1]

print("We will be compiling and running your own Kazm file: " + sys.argv[1] + "\n")

def run(args):
    out = subprocess.run(args, capture_output=True)
    return out.returncode, out.stdout.decode("utf8"), out.stderr.decode("utf8")

print("Building Kazm & Compiling kazm.native")
code, stdout, stderr = run(["opam", "config", "exec", "--", "ocamlbuild", "-use-ocamlfind", "kazm.native"])
if code != 0:
    print("Failed to compile kazm.native")
    print(stderr)
    print(stdout)
    exit(1)
print("Compiling builtins")
builtins_o = "/builtins.o"
code, _, _ = run(["cc", "-c", "builtins.c", "-o", builtins_o])
assert code == 0, "Failed to compile builtins"



