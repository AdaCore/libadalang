#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

from testsuite_support import LALTestsuite


if __name__ == '__main__':
    LALTestsuite().testsuite_main()
