#!/usr/bin/python3
"""
nofib-for-PureLang test driver.

Compiles each benchmark with the PureCake toolchain (via the examples Makefile),
runs it with the workload for the chosen mode (fast/norm/slow), and checks the
output against the recorded reference (<name>.faststdout / .stdout / .slowstdout).

Examples:
    ./run.py                      # compile, run and check every benchmark (norm)
    ./run.py --mode fast          # use the fast workloads
    ./run.py -k tak -k primes     # only the named benchmarks
    ./run.py --time --iterations 5  # also report wall-clock time
    ./run.py --no-compile         # reuse already-built executables

Compilation uses ../../Makefile, so it relies on a working PureCake `pure.S`
(built under ../../../compiler/binary, or downloaded with `make download`) and a
compatible CakeML `cake`. See README.md.
"""

import argparse
import configparser
import os
import subprocess
import sys
import time

HERE = os.path.dirname(os.path.abspath(__file__))            # .../examples/benchmark/nofib
EXAMPLES = os.path.abspath(os.path.join(HERE, "..", ".."))   # .../examples
SUBSETS = ["imaginary", "spectral"]
MODE_EXT = {"fast": "faststdout", "norm": "stdout", "slow": "slowstdout"}

GREEN, RED, BOLD, RESET = "\033[0;32m", "\033[0;31m", "\033[1m", "\033[0m"


def discover():
    """Yield (subset, name, dir) for every benchmark, in sorted order."""
    for subset in SUBSETS:
        root = os.path.join(HERE, subset)
        if not os.path.isdir(root):
            continue
        for name in sorted(os.listdir(root)):
            d = os.path.join(root, name)
            if os.path.isfile(os.path.join(d, name + ".hs")):
                yield subset, name, d


def arg_for(d, mode):
    cfg = configparser.ConfigParser()
    # sizes.cfg has no section header; wrap it.
    with open(os.path.join(d, "sizes.cfg")) as f:
        cfg.read_string("[sizes]\n" + f.read())
    return cfg.get("sizes", mode)


def compile_bench(subset, name, pureopt):
    target = os.path.join("benchmark", "nofib", subset, name, name + ".exe")
    env = os.environ.copy()
    env["PUREOPT"] = pureopt
    r = subprocess.run(["make", "-C", EXAMPLES, target], env=env,
                       capture_output=True, text=True)
    return r.returncode == 0, r.stderr


def run_once(exe, arg, heap):
    env = os.environ.copy()
    env["CML_HEAP_SIZE"] = str(heap)
    start = time.time()
    r = subprocess.run([exe, arg], env=env, capture_output=True, text=True)
    return r.returncode, r.stdout, time.time() - start


def main():
    ap = argparse.ArgumentParser(description="nofib-for-PureLang test driver.")
    ap.add_argument("--mode", choices=list(MODE_EXT), default="norm",
                    help="workload size (default: %(default)s)")
    ap.add_argument("-k", "--keep", action="append", default=[],
                    help="only run benchmarks whose name contains this (repeatable)")
    ap.add_argument("--no-compile", action="store_true",
                    help="reuse existing executables instead of rebuilding")
    ap.add_argument("--time", action="store_true",
                    help="report wall-clock time (min over --iterations runs)")
    ap.add_argument("--iterations", type=int, default=1,
                    help="iterations when --time is given (default: %(default)s)")
    ap.add_argument("--pureopt", default="",
                    help="extra flags passed to the PureCake frontend (PUREOPT)")
    ap.add_argument("--heap", type=int, default=4096,
                    help="CakeML heap size in MB (default: %(default)s)")
    ap.add_argument("--list", action="store_true", help="list benchmarks and exit")
    args = ap.parse_args()

    benches = [b for b in discover()
               if not args.keep or any(k in b[1] for k in args.keep)]

    if args.list:
        for subset, name, _ in benches:
            print("%-10s %s" % (subset, name))
        return 0

    ext = MODE_EXT[args.mode]
    npass = nfail = 0
    print("%sPureLang nofib — mode=%s, heap=%dMB%s\n" %
          (BOLD, args.mode, args.heap, RESET))

    for subset, name, d in benches:
        inp = arg_for(d, args.mode)
        label = "%s/%s" % (subset, name)

        if not args.no_compile:
            ok, err = compile_bench(subset, name, args.pureopt)
            if not ok:
                print("%-26s %sCOMPILE FAIL%s" % (label, RED, RESET))
                sys.stderr.write(err + "\n")
                nfail += 1
                continue

        exe = os.path.join(EXAMPLES, "out", "benchmark", "nofib",
                           subset, name, name + ".exe")
        rc, out, dt = run_once(exe, inp, args.heap)

        ref_path = os.path.join(d, name + "." + ext)
        ref = open(ref_path).read() if os.path.exists(ref_path) else None

        if rc != 0:
            status = "%sRUN FAIL (rc=%d)%s" % (RED, rc, RESET)
            nfail += 1
        elif ref is None:
            status = "%sNO REFERENCE%s" % (RED, RESET)
            nfail += 1
        elif out == ref:
            status = "%sPASS%s" % (GREEN, RESET)
            npass += 1
        else:
            status = "%sFAIL (output mismatch)%s" % (RED, RESET)
            nfail += 1

        timing = ""
        if args.time and rc == 0:
            best = dt
            for _ in range(max(0, args.iterations - 1)):
                best = min(best, run_once(exe, inp, args.heap)[2])
            timing = "  %.3fs" % best

        print("%-26s arg=%-22s %s%s" % (label, inp, status, timing))

    print("\n%s%d passed, %d failed%s" %
          (BOLD, npass, nfail, RESET))
    return 1 if nfail else 0


if __name__ == "__main__":
    sys.exit(main())
