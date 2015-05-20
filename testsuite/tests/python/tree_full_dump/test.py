from libadalang import AnalysisContext, ASTNode, Token


def dump(node, indent=''):
    if node is None:
        print '{}<null node>'.format(indent)
        return

    print '{}<{}>'.format(indent, node.kind_name)
    indent = indent + '|'
    for name, value in node.iter_fields():
        if isinstance(value, ASTNode):
            print '{}{}:'.format(indent, name)
            dump(value, indent + '  ')
        elif isinstance(value, Token):
            print '{}{}: Token({})'.format(indent, name, repr(value.text))
        else:
            print '{}{}: {}'.format(indent, name, value)

ctx = AnalysisContext()
unit = ctx.get_from_file('foo.adb')
dump(unit.root)
print 'Done'
