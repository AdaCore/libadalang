#! /bin/sh

# Build Libadalang for code coverage (requires GNATcoverage) and run the
# testsuite to compute the coverage report. Note that we keep internal
# testcases out of the scope of code coverage, as for this we focus on feature
# tests.
#
# This special build happens in the "build-cov" directory and the coverage
# report is produced in the "cov" directory.

set -e
set -x

ada/manage.py --build-dir=build-cov make -P --coverage
ada/manage.py --build-dir=build-cov test -- \
    --coverage cov --skip-internal-tests
