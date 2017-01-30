import os
import sys


sys.path.append(os.path.join(
    os.environ['LIBADALANG_ROOTDIR'],
    '..', 'contrib'
))
import check_same_operands
check_same_operands.main(check_same_operands.parser.parse_args(
    ['foo.adb']))
