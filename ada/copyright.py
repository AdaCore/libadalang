#! /usr/bin/env python

"""
Helpers to format copyright headers.
"""

import sys


BOX_SIZE = 78
TEXT_SIZE = 72


copyright = 'Copyright (C) 2014-2022, AdaCore'

header = f"""
{copyright}
SPDX-License-Identifier: Apache-2.0

""".strip().splitlines()


def concat(header, source):
    return '\n'.join(header) + '\n\n' + source.lstrip()


def format_start(prefix):
    result = [prefix.strip()]
    for line in header:
        result.append(f"{prefix}{line}")
    result.append(prefix.strip())
    return result


def format_ada(source):
    return concat(format_start('--  '), source)


def format_python(source):
    # If there is a shebang, add the copyright header after it
    if source.startswith('#!'):
        shebang, rest = source.split('\n', 1)
        shebang += '\n\n'
    else:
        shebang = ''
        rest = source
    return shebang + concat(format_start('# '), rest)


def format_tags(source, opening, closing):
    prefix = " *" + " " * (len(opening) - 1)

    result = [opening]
    for i, line in enumerate(header):
        result.append(prefix + line)
    result.append(closing)
    return concat(result, source)


def format_ocaml(source):
    return format_tags(source, "(*", " *)")


def format_c(source):
    return format_tags(source, "/*", " */")


def run(argv):
    for filename in argv:
        _, ext = filename.rsplit('.', 1)
        formatter = {
            'ads': format_ada,
            'adb': format_ada,
            'c': format_c,
            'h': format_c,
            'py': format_python,
        }[ext]

        with open(filename, 'rb') as f:
            content = f.read()

        with open(filename, 'wb') as f:
            f.write(formatter(content))


if __name__ == '__main__':
    run(sys.argv[1:])
