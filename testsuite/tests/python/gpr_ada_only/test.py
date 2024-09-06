import libadalang as lal


for ada_only in [False, True]:
    print("== {} ==".format("All languages" if ada_only else "Ada only"))
    print("")

    try:
        prj = lal.GPRProject("foo.gpr", ada_only=ada_only, print_errors=False)
    except lal.ProjectError as exc:
        print(f"Got an exception: {type(exc).__name__}: {exc}")
        print("")
    else:
        print("Got no exception")
        print("")
        print("Errors:")
        for e in prj.errors:
            print(e)
        print("")


print("Done")
