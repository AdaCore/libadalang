#! /usr/bin/env python

from __future__ import absolute_import, division, print_function

import os
from os.path import join
import sys

ADA_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = join(ADA_DIR, '..')

DIRS = ('ada', 'utils')
EXCLUDES = ('tmp', 'doc',
            join('testsuite', 'acats'),
            join('testsuite', 'tests', 'contrib'))

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
