import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file('test.adb')

for eh in u.root.findall(lal.ExceptionHandler):
    print("relative_name = {}".format(eh.p_relative_name))
    print("relative name text = {}".format(repr(eh.p_relative_name_text)))
