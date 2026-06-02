import libadalang as lal


prj = lal.GPRProject("foo.gpr")

for line_mode in [None] + list(lal.FileReader.LineMode):
    print(f"== line_mode={line_mode} ==")
    print("")

    fr = prj.create_preprocessor(line_mode=line_mode)
    ctx = lal.AnalysisContext(file_reader=fr)
    u = ctx.get_from_file("foo.adb")
    for i, line in enumerate(u.text.split("\n"), 1):
        print(f"  {str(i).rjust(2)} | {line}".rstrip())

    print("")

print("Done")
