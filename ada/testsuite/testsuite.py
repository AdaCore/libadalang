#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os

from testsuite_support import Testsuite


if __name__ == '__main__':
    Testsuite(os.path.dirname(__file__)).testsuite_main()
