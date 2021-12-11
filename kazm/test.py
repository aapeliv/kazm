#!/bin/python3


"""
This is the hackiest, shittiest test runner ever.
"""

import difflib
from glob import glob
from pathlib import Path
import shutil
import subprocess

# clean test build dir
tmp_dir = Path(".") / "_tests_build"
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
code, stdout, _ = run(["opam", "config", "exec", "--", "ocamlbuild", "-use-ocamlfind", "kazm.native"])
if code != 0:
    print("Failed to compile kazm.native")
    print(stdout)
    exit(1)
print(f" Compiling builtins")
builtins_o = f"{tmp_dir}/builtins.o"
code, _, _ = run(["cc", "-c", "builtins.c", "-o", builtins_o])
assert code == 0, "Failed to compile builtins"


tests = []

class TestException(Exception):
    pass

class TestFailed(TestException):
    pass

class TestPassed(TestException):
    pass

for filename in sorted(glob("tests/*.kazm")):
    assert filename.startswith("tests/")
    assert filename.endswith(".kazm")
    name = filename[len("tests/"):-len(".kazm")]
    out_file = Path(f"tests/{name}.out")
    run_err_file = Path(f"tests/{name}.err")
    compile_err_file = Path(f"tests/{name}.cerr")
    assert out_file.exists() or run_err_file.exists() or compile_err_file.exists(), f"Test '{name}' must pass, fail, or fail to compile"
    tests.append((filename, name, out_file, run_err_file, compile_err_file))

print(f"Picked up {len(tests)} tests.")

test_output = f"\n\n{'':*<33} Test results {'':*<33}\n\n"

def green(msg):
    return "\033[32m" + msg + "\033[0m"

def red(msg):
    return "\033[31m" + msg + "\033[0m"

def gen_diff(output, expected):
    return "\n".join(
                difflib.unified_diff(
                    expected.strip().splitlines(),
                    output.strip().splitlines(),
                    fromfile="expected",
                    tofile="output",
                )
            )

failed = 0
passed = 0

for filename, name, out_file, run_err_file, compile_err_file in tests:
    description = None
    output = None
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
        passed += 1
    except TestFailed as e:
        status = "fail"
        failed += 1
        description = str(e)

    # print nice output
    color = green if status == "pass" else red
    print(color(f"  ...{status}"))
    test_output += color(f"{name:.<40}{status:.>40}") + "\n"
    if description:
        test_output += color(f"{description:>80}\n")
    if output:
        test_output += "\n" + "\n".join([f"    {line}" for line in output.splitlines()]) + "\n\n"


total_tests = len(tests)
assert passed + failed == total_tests
test_output += "\n\n\n"
test_output += red(f"Fail{failed:.>21} tests") + "\n"
test_output += green(f"Pass{passed:.>21} tests") + "\n"
test_output += f"Total{total_tests:.>20} tests\n"

print(test_output)