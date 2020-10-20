# Set this manually to True to generate a coverage report for laldoc
COVERAGE = False

if COVERAGE:
    import coverage

import sys

from os import path as P
from utils import in_contrib


if COVERAGE:
    cov = coverage.Coverage()

sys.path.append(P.join(in_contrib(), 'laldoc', 'laldoc'))
import generate_rst


print("Generating rst")
print("==============")
print()

if COVERAGE:
    cov.start()
generate_rst.GenerateDoc.run(['base.ads'])
if COVERAGE:
    cov.stop()

print()
print("Output")
print("======")

with open('base.rst') as f:
    print(f.read())

if COVERAGE:
    cov.save()
    cov.html_report()
