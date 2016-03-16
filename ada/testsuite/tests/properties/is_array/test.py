import libadalang

import source


with open('pack.ads') as f:
    src_file = f.read()
src_file_lines = src_file.splitlines()

ctx = libadalang.AnalysisContext()
unit = ctx.get_from_buffer('pack.ads', src_file)
assert not unit.diagnostics, 'Got diagnostics: {}'.format(unit.diagnostics)
unit.populate_lexical_env()


def src_slice(node):
    return source.src_slice(src_file_lines, node.sloc_range)


for obj_decl in unit.root.finditer(libadalang.ObjectDecl):
    print(src_slice(obj_decl))
    print('    is_array = {}'.format(obj_decl.p_is_array))
