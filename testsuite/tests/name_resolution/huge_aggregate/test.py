import libadalang as lal


# We only start having trouble for values above ~1_000_000
N = 1_000_000

assocs = ["0" for i in range(1, N)]
agg = ", ".join(assocs)

test_content = f"""
procedure Test is
    X : array (1 .. {N}) of Integer := ({agg});
begin
    null;
end Test;
"""

ctx = lal.AnalysisContext()
u = ctx.get_from_buffer("test.adb", test_content)
print("Resolution of aggregate is successful: ", end='')
print(u.root.find(lal.ObjectDecl).p_resolve_names)
