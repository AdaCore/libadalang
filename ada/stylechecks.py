#! /usr/bin/env python

import os
from os.path import join
import sys


def in_test(*args):
    return join('testsuite', 'tests', *args)


ADA_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = join(ADA_DIR, '..')

DIRS = ['ada', 'contrib', 'utils']
EXCLUDES = ['tmp', 'doc',
            join('contrib', 'highlight', 'obj'),
            join('contrib', 'AdaEurope2018', 'obj'),
            join('testsuite', 'ext_src'),
            in_test('contrib'),
            in_test('internal'),
            in_test('ada_api', 'static_expr_eval', 'test.adb'),
            in_test('python', 'char_literal', 'foo.ads'),
            in_test('python', 'string_literal', 'foo.ads'),
            in_test('name_resolution', 'symbol_canon'),
            in_test('regressions')]

sys.path.append(join(ROOT_DIR, 'langkit'))

import langkit.stylechecks


def main():
    langkit.stylechecks.main(ROOT_DIR, sys.argv[1:], DIRS, EXCLUDES)


if __name__ == '__main__':
    main()
