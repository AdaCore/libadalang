#! /usr/bin/env python

"""
Helpers to format copyright headers.
"""

from __future__ import absolute_import, division, print_function

import sys


BOX_SIZE = 78
TEXT_SIZE = 72


def centered(text):
    return ' ' * ((TEXT_SIZE - len(text)) // 2) + text


name = 'Libadalang'
copyright = 'Copyright (C) 2014-2018, AdaCore'

header = """{}

{}

Libadalang is free software;  you can redistribute it and/or modify  it
under terms of the GNU General Public License  as published by the Free
Software Foundation;  either version 3,  or (at your option)  any later
version.   This  software  is distributed in the hope that it  will  be
useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of
MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.   See the  GNU
General  Public  License for more details.   You should have received a
copy of the GNU General Public License  distributed with this software;
see  file  COPYING3.  If not, go to  http://www.gnu.org/licenses  for a
complete copy of the license.
""".format(centered(name), centered(copyright)).splitlines()


def concat(header, source):
    return '\n'.join(header) + '\n\n' + source.lstrip()


def format_start(start_char):
    result = []
    result.append(start_char * BOX_SIZE)
    result.append('{c}{c}{padding}{c}{c}'.format(
        c=start_char,
        padding=' ' * (BOX_SIZE - 4)
    ))
    for line in header:
        result.append('{c}{c} {text} {c}{c}'
                      .format(c=start_char, text=line.ljust(TEXT_SIZE)))
    result.append(start_char * BOX_SIZE)
    return result


def format_ada(source):
    return concat(format_start('-'), source)


def format_python(source):
    # If there is a shebang, add the copyright header after it
    if source.startswith('#!'):
        shebang, rest = source.split('\n', 1)
        shebang += '\n\n'
    else:
        shebang = ''
        rest = source
    return shebang + concat(format_start('#'), rest)


def format_c(source):
    result = []
    for i, line in enumerate(header):
        head = '   '
        tail = '   '
        if i == 0:
            head = '/* '
        elif i == len(header) - 1:
            tail = ' */'
        result.append((head + line + tail).rstrip())
    return concat(result, source)


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
