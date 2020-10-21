import libadalang


def short_repr(node):
    return ('{}[{}]'.format(node.kind_name, node.sloc_range)
            if node else
            'None')


ctx = libadalang.AnalysisContext('utf-8')
unit = ctx.get_from_file('foo.adb')

unit.root.dump()

nodes = [unit.root] + list(
    unit.root.findall(lambda n: isinstance(n, libadalang.SubpSpec))
)
for node in nodes:
    print('Parents of {}:'.format(short_repr(node)))
    for parent in node.parents:
        print('  - {}'.format(short_repr(parent)))
