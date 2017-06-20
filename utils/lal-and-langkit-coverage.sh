#! /bin/sh

# First run the Langkit testuite in coverage mode:
#    [in lankit/] $ scripts/interactive_testsuite -C
# And then run this script in libadalang/

COVERAGE_FILE=lal.coverage coverage run \
   --rcfile=langkit/testsuite/coverage.ini \
   --source=langkit/langkit/ \
   ada/manage.py generate

coverage combine lal.coverage $(find langkit -name .coverage)

coverage html \
   --rcfile=langkit/testsuite/coverage.ini \
   -d coverage-report \
   --title='Langkit coverage report consolidated with Libadalang'
