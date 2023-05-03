import sys

import libadalang as lal


def flush():
    sys.stdout.flush()
    sys.stderr.flush()


def run(project_file, **kwargs):
    print("== {}: {} ==".format(
        project_file,
        ", ".join(f"{k}={v}" for k, v in sorted(kwargs.items()))
    ))
    print("")
    print("--- BEFORE loading")
    flush()

    try:
        prj = lal.GPRProject(project_file, **kwargs)
        flush()
    except lal.ProjectError as exc:
        print(f"Got an exception: {type(exc).__name__}: {exc}")
        print("")
    else:
        print("--- AFTER loading")
        print("")
        print("Errors:")
        for e in prj.errors:
            print(e)
        print("")


run("nosuchgpr.gpr")
run("nosuchtarget.gpr")
run("nosuchtarget.gpr", print_errors=False)
run("nosuchtarget.gpr", print_errors=True)
run("missingdep.gpr")

print("Done")
