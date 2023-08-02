import libadalang as lal


ctx = lal.AnalysisContext()


def load(filename: str):
    u = ctx.get_from_file(filename)
    assert not u.diagnostics
    return u


def check_pragmas():
    for filename in ["foo.adb", "bar.adb"]:
        print(f"Pragmas for {filename}:")
        u = load(filename)

        try:
            pragmas = u.root.p_all_config_pragmas
        except lal.PropertyError as exc:
            print("Got a PropertyError:", exc)
            continue

        for p in pragmas:
            print("Config pragma:", p)


global_p = load("global.adc")
local_1 = load("local_1.adc")
local_2 = load("local_2.adc")

foo = load("foo.adb")
bar = load("bar.adb")


print("== Query config pragmas with no mappings set ==")
check_pragmas()
print("")

print("== Set empty mappings ==")
ctx.set_config_pragmas_mapping(None, {})
check_pragmas()
print("")

print("== Set global-only mapping ==")
ctx.set_config_pragmas_mapping(global_p, {})
check_pragmas()
print("")

print("== Set local mapping for foo.adb ==")
ctx.set_config_pragmas_mapping(global_p, {foo: local_1})
check_pragmas()
print("")

print("== Set all mappings ==")
ctx.set_config_pragmas_mapping(global_p, {foo: local_1, bar: local_2})
check_pragmas()
print("")

print("== Bad type (error) ==")
try:
    ctx.set_config_pragmas_mapping(global_p, {foo: None})
except AssertionError as exc:
    print("Got an assertion failure:", exc)
check_pragmas()
print("")

print("Done.")
