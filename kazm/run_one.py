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

name = my_file[:-len(".kazm")]
out_file = Path(f"{name}.out")
run_err_file = Path(f"{name}.err")
compile_err_file = Path(f"{name}.cerr")
assert out_file.exists() or run_err_file.exists() or compile_err_file.exists(), f"Test '{name}' must pass, fail, or fail to compile"

class TestException(Exception):
    pass

class TestFailed(TestException):
    pass

class TestPassed(TestException):
    pass

def gen_diff(output, expected):
    return "\n".join(
                difflib.unified_diff(
                    expected.strip().splitlines(),
                    output.strip().splitlines(),
                    fromfile="expected",
                    tofile="output",
                )
            )


try:
        print(f"Running test '{name}'")
        # LLVM IR file
        ll_file = f"{tmp_dir}/{name}.ll"
        # assembly file
        s_file = f"{tmp_dir}/{name}.s"
        # executable
        ex_file = f"{tmp_dir}/{name}"
        print(f" Compiling '{name}' through kazm...")
        with open(filename, "rb") as f:
            code, data, err = pipe_through(["./kazm.native"], f.read())
            # failed to compile
        if code != 0:
            stdout = err.decode("utf8")
            if not compile_err_file.exists():
                # wasn't supposed to fail to compile
                output = stdout
                raise TestFailed("failed to compile with kazm")
            else:
                # failed to compile as expected, check outputs match
                diff = gen_diff(stdout, compile_err_file.read_text())
                if diff != "":
                    output = "diff:\n" + diff
                    raise TestFailed("wrong compile error from kazm")
                else:
                    raise TestPassed()
        with open(ll_file, "wb") as o:
            o.write(data)
        print(f" Compiling '{name}' through LLVM...")
        code, _, stderr = run(["llc", "--relocation-model=pic", ll_file, f"-o={s_file}"])
        # failed to compile llvm
        if code != 0:
            output = stderr
            raise TestFailed("failed to compile with llvm")
        print(f" Linking '{name}' through cc...")
        code, _, stderr = run(["cc", "-o", ex_file, builtins_o, s_file])
        if code != 0:
            output = stderr
            raise TestFailed("failed to link")
        # actually run the code
        code, stdout, stderr = run([ex_file])
        if code == 0:
            if not out_file.exists():
                # it ran but didn't expect it to run
                raise TestFailed("didn't expect to run but it did")
            else:
                # ran and expected to run
                diff = gen_diff(stdout, out_file.read_text())
                if diff != "":
                    output = "diff:\n" + diff
                    raise TestFailed("ran but with wrong output")
        else:
            # didn't run
            output = f"code: {code}\n" + stderr
            raise TestFailed("returned with non-zero exit code")
        raise TestPassed()
except TestPassed:
    status = "pass"
    print("PASSSED")
except TestFailed as e:
    status = "fail"
    description = str(e)
    print("FAILED")


