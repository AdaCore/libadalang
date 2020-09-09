import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('test.adb')

for eh in u.root.findall(lal.ExceptionHandler):
    print(f"relative_name = {eh.p_relative_name}")
    print(f"relative name text = {repr(eh.p_relative_name_text)}")
