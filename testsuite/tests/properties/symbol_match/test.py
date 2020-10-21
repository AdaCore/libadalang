import libadalang

import source


with open('foo.adb') as f:
    src_file = f.read()
src_file_lines = src_file.splitlines()


def src_slice(node):
    return source.src_slice(src_file_lines, node.sloc_range)


ctx = libadalang.AnalysisContext()
unit = ctx.get_from_buffer('foo.adb', src_file)
assert not unit.diagnostics, 'Got diagnostics: {}'.format(unit.diagnostics)


identifiers = unit.root.findall(libadalang.Identifier)
foo, i = identifiers[:2]

for a, b in [
    (foo, foo),
    (foo, i),
]:
    print('{} matches {} => {}'.format(a, b, a.p_name_matches(b)))
