#! /usr/bin/env python

"""
Helpers to format copyright headers.
"""

import sys

from langkit.utils import SourcePostProcessor


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


class AdaPostProcessor(SourcePostProcessor):
    def process(self, source):
        return concat(format_start('--  '), source)


class PythonPostProcessor(SourcePostProcessor):
    def process(self, source):
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


class OCamlPostProcessor(SourcePostProcessor):
    def process(self, source):
        return format_tags(source, "(*", " *)")


class CCppPostProcessor(SourcePostProcessor):
    def process(self, source):
        return format_tags(source, "/*", " */")


def run(argv):
    for filename in argv:
        _, ext = filename.rsplit('.', 1)
        cls = {
            'ads': AdaPostProcessor,
            'adb': AdaPostProcessor,
            'c': CCppPostProcessor,
            'h': CCppPostProcessor,
            'py': PythonPostProcessor,
        }[ext]

        with open(filename, 'rb') as f:
            content = f.read()

        with open(filename, 'wb') as f:
            f.write(cls().process(content))


if __name__ == '__main__':
    run(sys.argv[1:])
