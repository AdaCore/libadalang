import libadalang as lal


SPEC = lal.AnalysisUnitKind.unit_specification
BODY = lal.AnalysisUnitKind.unit_body


def check(label, tests, input_files, charset=None):
    print(f"== {label} ==")
    print("")
    up = lal.UnitProvider.auto(input_files, charset=charset)
    ctx = lal.AnalysisContext(unit_provider=up)
    for unit_name, unit_kind in tests:
        unit = ctx.get_from_provider(unit_name, unit_kind)
        if unit.diagnostics:
            for d in unit.diagnostics:
                print(d)
        else:
            print(f"{unit_name}/{unit_kind}: {unit.root}")
    print("")


check(
    "default charset",
    tests=[("foo", BODY), ("foo", SPEC), ("bar", BODY)],
    input_files=["foo-utf8.ada"],
)
check(
    "utf-8",
    tests=[("foo", BODY)],
    input_files=["foo-utf8.ada", "foo-utf16.ada"],
    charset="utf-8",
)
check(
    "utf-16",
    tests=[("foo", BODY)],
    input_files=["foo-utf8.ada", "foo-utf16.ada"],
    charset="utf-16",
)

print("Done.")
