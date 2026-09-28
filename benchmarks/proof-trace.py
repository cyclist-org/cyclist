#!/usr/bin/env python3
"""Record the proofs, or other evidence, that cyclist gives for a fixed set of
benchmark cases, as logs fit for regression testing.

  proof-trace.py select -o CASES  pick the cases to trace
  proof-trace.py run [DIR...]     trace the cases
  proof-trace.py check [DIR...]   trace the cases and compare with the baselines
  proof-trace.py replace [DIR...] accept the last traces as the baselines

The benchmark Makefiles list every case (`make -C benchmarks cases`); `select`
keeps those that finish, proved or not, well within the timeout, and whose
output is the same on a second run. That choice depends on the speed of the
host, which is why it is made once and committed, as benchmarks/proof-trace.cases,
rather than remade on every `run`: the logs should only change when cyclist
does.

Each benchmark directory gets a log of its own: `run` writes the trace of its
cases to proof-trace.new.log in that directory, and `check` compares it with
the committed baseline proof-trace.log beside it. A DIR, relative to
benchmarks/, restricts these to the directories at or under it (sl covers
sl/base, sl/songbird and sl/atva-2014); with none, they cover every directory.

A CASES file holds one case per line: its name, the benchmark directory it
comes from, then the cyclist arguments that run it, all followed by a tab.
Lines starting with # are comments.
"""

import argparse
import os
import re
import shlex
import shutil
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXE = os.path.join(ROOT, "_build", "default", "src", "cli", "cyclist.exe")
CASES = os.path.join("benchmarks", "proof-trace.cases")
# The names of the logs in each benchmark directory.
TRACE = "proof-trace.new.log"
BASELINE = "proof-trace.log"

TIMEOUT = 60
# Cases that take longer than this are dropped by `select`, so that a slower
# host, or a loaded one, does not turn them into timeouts.
MAX_SECS = 15
# How long past TIMEOUT to wait before killing a case, since checkproof takes
# no timeout and a prover's own may fail to fire.
GRACE = 5

# Exit codes of a case that ran to a verdict: proved/SAT/valid, not
# proved/UNSAT, and invalid (sl prove). A timeout exits with 2, and so does an
# uncaught exception.
VERDICTS = {0, 1, 255}

# Lines that vary from run to run, dropped from the output.
NOISE = re.compile(r"^(Execution time:|z3 called|Total time taken)")


def evidence_flags(args):
    """The flags that make the case print its evidence, and bound its run."""
    if args[0] == "checkproof":
        return []
    return ["-p", "-t", str(TIMEOUT)]


def build():
    subprocess.run(["dune", "build", "src/cli/cyclist.exe"], cwd=ROOT, check=True)


def all_cases():
    out = subprocess.run(
        ["make", "-s", "--no-print-directory", "-C", "benchmarks", "cases"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    return [parse_case(line) for line in out.splitlines()]


def parse_case(line):
    fields = line.split("\t")
    if fields[-1] == "":
        fields.pop()
    return fields[0], fields[1], fields[2:]


def read_cases(path):
    with open(path) as f:
        return [
            parse_case(line.rstrip("\n"))
            for line in f
            if line.strip() and not line.startswith("#")
        ]


def format_case(name, where, args):
    return "".join(field + "\t" for field in [name, where] + args)


def run_case(args):
    """Run a case; return its exit code (None if killed), its normalised
    output, and its wall-clock time."""
    # With TERM set, cyclist lays proofs out to the width of the terminal.
    env = dict(os.environ, LC_ALL="C", TERM="dumb")
    env.pop("OCAMLRUNPARAM", None)
    start = time.monotonic()
    try:
        p = subprocess.run(
            [EXE] + args + evidence_flags(args),
            cwd=ROOT,
            env=env,
            stdin=subprocess.DEVNULL,
            capture_output=True,
            text=True,
            timeout=TIMEOUT + GRACE,
        )
    except subprocess.TimeoutExpired:
        return None, "", time.monotonic() - start
    secs = time.monotonic() - start
    out = normalise(p.stdout)
    err = normalise(p.stderr)
    if err:
        out += "--- stderr\n" + err
    return p.returncode, out, secs


def normalise(text):
    lines = [l.rstrip() for l in text.splitlines() if not NOISE.match(l)]
    return "".join(l + "\n" for l in lines)


def progress(cases):
    """Enumerate the cases, printing each directory as the cases reach it."""
    last = None
    for i, (name, where, args) in enumerate(cases, 1):
        if where != last:
            print(f"proof-trace: {where}", file=sys.stderr)
            last = where
        yield i, (name, where, args)


def select(out_path):
    cases = all_cases()
    kept = []
    for i, (name, where, args) in progress(cases):
        code, out, secs = run_case(args)
        if code in VERDICTS and secs < MAX_SECS:
            code2, out2, secs2 = run_case(args)
            secs = max(secs, secs2)
            if (code2, out2) != (code, out):
                why = "output differs between runs"
            elif secs >= MAX_SECS:
                why = "too slow on second run"
            else:
                why = None
        elif code is None:
            why = "killed"
        elif code not in VERDICTS:
            why = f"exit {code}"
        else:
            why = "too slow"
        status = "kept" if why is None else f"dropped: {why}"
        print(f"[{i}/{len(cases)}] {name}: {secs:.1f}s, {status}", file=sys.stderr)
        if why is None:
            kept.append((name, where, args))
    write(
        out_path,
        f"# Generated by `make proof-trace-select`: the cases below reached a\n"
        f"# verdict in under {MAX_SECS:g}s, twice, with the same output.\n"
        + "".join(format_case(*case) + "\n" for case in kept),
    )
    print(f"kept {len(kept)} of {len(cases)} cases", file=sys.stderr)


def chosen_cases(dirs):
    """The cases in CASES under any of dirs, or all of them if there are none."""
    cases = read_cases(os.path.join(ROOT, CASES))
    if not dirs:
        return cases
    prefixes = [os.path.join("benchmarks", os.path.normpath(d)) for d in dirs]
    chosen = [
        case
        for case in cases
        if any(case[1] == p or case[1].startswith(p + "/") for p in prefixes)
    ]
    for d, p in zip(dirs, prefixes):
        if not any(case[1] == p or case[1].startswith(p + "/") for case in chosen):
            sys.exit(f"proof-trace: no cases under benchmarks/{d}")
    return chosen


def directories(cases):
    return list(dict.fromkeys(where for _, where, _ in cases))


def run(dirs, quiet):
    cases = chosen_cases(dirs)
    logs = {}
    for i, (name, where, args) in progress(cases):
        code, out, _ = run_case(args)
        exit_line = "killed" if code is None else str(code)
        if not quiet:
            print(f"[{i}/{len(cases)}] {name}: exit {exit_line}", file=sys.stderr)
        cmd = shlex.join(["cyclist"] + args + evidence_flags(args))
        entry = f"=== {name}\n$ {cmd}\nexit: {exit_line}\n{out}\n"
        logs.setdefault(where, []).append(entry)
    for where, entries in logs.items():
        write(os.path.join(ROOT, where, TRACE), "".join(entries))


def check(dirs):
    """Print the differences from the baselines; return whether there are any."""
    run(dirs, quiet=True)
    differs = False
    for where in directories(chosen_cases(dirs)):
        base, trace = os.path.join(where, BASELINE), os.path.join(where, TRACE)
        if not os.path.exists(os.path.join(ROOT, base)):
            print(f"proof-trace: no baseline {base}")
            differs = True
            continue
        sys.stdout.flush()
        differs |= subprocess.run(["diff", "-u", base, trace], cwd=ROOT).returncode != 0
    return differs


def replace(dirs):
    for where in directories(chosen_cases(dirs)):
        trace = os.path.join(ROOT, where, TRACE)
        if not os.path.exists(trace):
            sys.exit(f"proof-trace: no trace {os.path.join(where, TRACE)}; run it first")
        shutil.copyfile(trace, os.path.join(ROOT, where, BASELINE))


def write(path, text):
    """Write the file whole, so that a failed run leaves the old one alone."""
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        f.write(text)
    os.replace(tmp, path)


def main():
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    sub = parser.add_subparsers(dest="mode", required=True)
    p = sub.add_parser("select", help="pick the cases to trace")
    p.add_argument("-o", "--output", required=True)
    p = sub.add_parser("run", help="trace the cases")
    p.add_argument("dirs", nargs="*", metavar="DIR")
    p.add_argument(
        "-q", "--quiet", action="store_true", help="report directories, not cases"
    )
    p = sub.add_parser("check", help="trace the cases and compare with the baselines")
    p.add_argument("dirs", nargs="*", metavar="DIR")
    p = sub.add_parser("replace", help="accept the last traces as the baselines")
    p.add_argument("dirs", nargs="*", metavar="DIR")
    opts = parser.parse_args()
    if opts.mode == "replace":
        replace(opts.dirs)
        return
    build()
    if opts.mode == "select":
        select(opts.output)
    elif opts.mode == "run":
        run(opts.dirs, opts.quiet)
    elif check(opts.dirs):
        sys.exit(1)


if __name__ == "__main__":
    main()
