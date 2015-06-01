from libadalang import AnalysisContext, ASTNode, Token, ASTList


def dump(node, indent=''):

    def print_node(name, value):
        if isinstance(value, ASTNode):
            print '{}{}:'.format(indent, name)
            dump(value, indent + '  ')
        elif isinstance(value, Token):
            print '{}{}: Token({})'.format(indent, name, repr(value.text))
        else:
            print '{}{}: {}'.format(indent, name, value)

    if node is None:
        print '{}<null node>'.format(indent)
        return

    print '{}<{}>'.format(indent, node.kind_name)
    indent = indent + '|'
    if isinstance(node, ASTList):
        for i, value in enumerate(node):
            print_node("item {}".format(i), value)
    else:
        for name, value in node.iter_fields():
            print_node(name, value)

ctx = AnalysisContext()
unit = ctx.get_from_file('foo.adb')
dump(unit.root)
print 'Done'
