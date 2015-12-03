#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

from testsuite_support import Testsuite
import os


if __name__ == '__main__':
    Testsuite(os.path.dirname(__file__)).testsuite_main()
