#! /usr/bin/env python

import os
from os.path import join
import sys

ADA_DIR = join(os.path.dirname(os.path.abspath(__file__)))
ROOT_DIR = join(ADA_DIR, '..')

DIRS = ('ada', 'langkit', 'utils')
EXCLUDES = ('tmp', 'doc', join('stylechecks', 'tests.py'),
            join('langkit', 'adalog', 'obj'),
            join('ada', 'testsuite', 'acats'))

sys.path.append(join(ROOT_DIR, "langkit"))

import langkit.stylechecks


def main():
    if sys.argv[1:]:
        langkit.stylechecks.main(sys.argv[1], None, None)
    else:
        os.chdir(ROOT_DIR)
        langkit.stylechecks.main(None, DIRS, EXCLUDES)

if __name__ == '__main__':
    main()
