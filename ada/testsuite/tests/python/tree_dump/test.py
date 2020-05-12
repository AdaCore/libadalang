from libadalang import AnalysisContext


def dump(node, indent=0):
    indent_str = '| ' * indent
    if node is None:
        print('{}<null node>'.format(indent_str))
        return

    print('{}<{}>'.format(indent_str, node.kind_name))
    for child in node:
        dump(child, indent + 1)

ctx = AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')
dump(unit.root)
print('Done')
