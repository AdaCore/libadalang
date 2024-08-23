import libadalang as lal


u = lal.AnalysisContext().get_from_buffer(
    "foo.adb", buffer=b"procedure Foo is begin null; end Foo;"
)
print(u.root)
