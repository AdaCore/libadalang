#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import os
from os.path import join
import sys


ADA_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = join(ADA_DIR, '..')

DIRS = ['ada', 'contrib', 'utils']
EXCLUDES = ['tmp', 'doc',
            join('contrib', 'highlight', 'obj'),
            join('contrib', 'AdaEurope2018', 'obj'),
            join('testsuite', 'ext_src'),
            join('testsuite', 'tests', 'contrib'),
            join('testsuite', 'tests', 'internal'),
            join('testsuite', 'tests', 'python', 'char_literal', 'foo.ads'),
            join('testsuite', 'tests', 'python', 'string_literal', 'foo.ads'),
            join('testsuite', 'tests', 'name_resolution', 'symbol_canon'),
            join('testsuite', 'tests', 'regressions')]

sys.path.append(join(ROOT_DIR, 'langkit'))

import langkit.stylechecks


def main():
    langkit.stylechecks.main(ROOT_DIR, sys.argv[1:], DIRS, EXCLUDES)


if __name__ == '__main__':
    main()
