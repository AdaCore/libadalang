import libadalang


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')

root = unit.root
assert root.kind_name == 'CompilationUnit'

all_ids = root.findall(libadalang.Identifier)
foo, i = all_ids[:2]

print('This should be Foo:')
foo.dump(indent='  ')

print('This should be I:')
i.dump(indent='  ')

print('Foo.p_name_matches(Foo) = {}'.format(foo.p_name_matches(foo)))
print('Foo.p_name_matches(I) = {}'.format(foo.p_name_matches(i)))

print('Done.')
