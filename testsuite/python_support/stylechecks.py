#! /usr/bin/env python

import os
from os.path import join
import sys


def in_shared(*args):
    return join('testsuite', 'shared', *args)


def in_test(*args):
    return join('testsuite', 'tests', *args)


python_support_dir = os.path.dirname(os.path.abspath(__file__))
root_dir = join(python_support_dir, '..', '..')

dirs = ['ada', 'contrib', 'testsuite', 'utils']
excludes = ['tmp', 'doc',
            join('contrib', 'highlight', 'obj'),
            join('contrib', 'AdaEurope2018', 'obj'),
            in_shared('gpr_context', 'simple', 'pkg.adb'),
            in_shared('gpr_context', 'utf-8', 'pkg.adb'),
            in_test('contrib'),
            in_test('internal'),
            in_test('ada_api', 'static_expr_eval', 'test.adb'),
            in_test('python', 'char_literal', 'foo.ads'),
            in_test('python', 'string_literal', 'foo.ads'),
            in_test('name_resolution', 'symbol_canon'),
            in_test('regressions')]

sys.path.append(join(root_dir, 'langkit'))

import langkit.stylechecks


def main():
    langkit.stylechecks.main(root_dir, sys.argv[1:], dirs, excludes)


if __name__ == '__main__':
    main()
