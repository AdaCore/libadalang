import libadalang as lal


ctx = lal.AnalysisContext(with_trivia=True)
unit = ctx.get_from_file('lal-highlighters.adb')
loc = lal.Sloc(1, 1)
t = unit.lookup_token(loc)
for i in range(40):
    print(t)
    t = t.next
