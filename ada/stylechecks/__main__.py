#! /usr/bin/env python

import os
from os.path import join
import sys

ADA_DIR = join(os.path.dirname(os.path.abspath(__file__)), '..')
ROOT_DIR = join(ADA_DIR, '..')

DIRS = ('ada', 'langkit', 'utils')
EXCLUDES = ('tmp', 'doc', join('stylechecks', 'tests.py'),
            join('langkit', 'adalog', 'obj'),
            join('ada', 'testsuite', 'acats'))

sys.path.append(ADA_DIR)

from stylechecks import Report, check_file, traverse


def main(args):
    os.chdir(ROOT_DIR)
    report = Report(enable_colors=os.isatty(sys.stdout.fileno()))
    if args:
        check_file(report, args[0])
    else:
        for root in DIRS:
            traverse(report, root, EXCLUDES)
    report.output()


if __name__ == '__main__':
    main(sys.argv[1:])
