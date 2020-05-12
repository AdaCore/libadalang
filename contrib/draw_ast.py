#! /usr/bin/env python

from argparse import ArgumentParser
from itertools import izip_longest
import sys

import libadalang as lal


parser = ArgumentParser(
    description='Draw an AST for the input Ada source file. The result is a'
                ' dot source file printed on the standard output.')
parser.add_argument('file', help='Ada source file to process')


def clamp(value, min, max):
    """Return "value", or min/max if it's out of range."""
    if value < min:
        return min
    elif value > max:
        return max
    else:
        return value


class Color(object):
    """Handy holder to compute colors."""
    def __init__(self, red, green, blue):
        self.red = clamp(red, 0, 255)
        self.green = clamp(green, 0, 255)
        self.blue = clamp(blue, 0, 255)

    def __add__(self, color):
        return Color(self.red + color.red,
                     self.green + color.green,
                     self.blue + color.blue)

    def __mul__(self, scalar):
        return Color(self.red * scalar,
                     self.green * scalar,
                     self.blue * scalar)

    def __repr__(self):
        return '#{:02x}{:02x}{:02x}'.format(int(self.red),
                                            int(self.green),
                                            int(self.blue))


WHITE = Color(255, 255, 255)
BLUE_AQUA = Color(0, 128, 255)

BACKGROUND = WHITE * 0.1
LINES = WHITE * 0.3
TITLE = WHITE * 0.2 + BLUE_AQUA * 0.4
REGULAR_LABEL = WHITE * 0.5
OTHER_LABEL = WHITE * 0.3


class NameGenerator(object):
    names_counter = 0
    names = {}

    @classmethod
    def node_name(cls, parent, node):
        """
        Return the name to give to a node.

        We use the parent so that each "None" node has its own identity.
        """
        key = (id(parent), id(node))
        try:
            value = cls.names[key]
        except KeyError:
            value = cls.names_counter
            cls.names[key] = value
            cls.names_counter += 1
        return 'node_{}'.format(value)


def edge(parent, node, child, port=None):
    """
    Dot code for an edge from "node" to "child".

    "parent" must be "node"'s parent. If the edge must come from a specific
    port in "node", then "port" must contain the name of this port.
    """
    return '{}{} -> {} [color="{}"];'.format(
        NameGenerator.node_name(parent, node),
        '' if port is None else ':{}'.format(port),
        NameGenerator.node_name(node, child),
        LINES
    )


def colored(text, color):
    """Dot code (HTML label format) for a colored text."""
    return '<font color="{}" face="Sans">{}</font>'.format(color, text)


def table(title, rows, ports=None):
    """
    Dot code (HTML label format) for a table with the given "title" and the
    given rows (a list of strings). If ports is provided, it must be a list of
    string to assign port names to rows.
    """
    result = [
        '<table color="#404040" cellborder="0">',
        '<tr><td><b>{}</b></td></tr>'.format(colored(title, TITLE))
    ]
    for row, port in izip_longest(rows, ports or []):
        result.append(
            '<hr/><tr><td align="left"{}>{}</td></tr>'.format(
                ' port="{}"'.format(port) if port else '',
                colored(row, REGULAR_LABEL)
            )
        )
    result.append('</table>')
    return ''.join(result)


def to_dot(parent, node):
    """
    Return a list of strings for a Dot source file that graphically describes
    the input "node" AST node, given its "parent".
    """
    result = []

    if isinstance(node, lal.Token):
        label = [table('Token', [node.text])]

    elif isinstance(node, lal.AdaNode):
        if node.is_list_type:
            label = [colored('&lt;list&gt;', OTHER_LABEL)]
            for child in node:
                result.extend(to_dot(node, child))
                result.append(edge(parent, node, child))
        else:
            label = []
            rows = []
            for name, value in node.iter_fields():
                rows.append(name)
                result.extend(to_dot(node, value))
                result.append(edge(parent, node, value, name))

            label.extend(table(type(node).__name__, rows, rows))

    else:
        label = [colored(str(node), OTHER_LABEL)]

    result.append(
        '{} [label=<{}>, shape=rectangle, penwidth=0];'.format(
            NameGenerator.node_name(parent, node),
            ''.join(label),
        )
    )
    return result


def main(args):
    ctx = lal.AnalysisContext()
    u = ctx.get_from_file(args.file)
    if u.diagnostics:
        for d in u.diagnostics:
            sys.stderr.write('{}\n'.format(d))
    if u.root:
        print('digraph g {')
        print('graph [rangkdir="LR", splines=true, bgcolor="{}",'
              ' fontname="Sans"];'.format(BACKGROUND))
        print('\n'.join(to_dot(None, u.root)))
        print('}')
    else:
        sys.exit(1)


if __name__ == '__main__':
    main(parser.parse_args())
