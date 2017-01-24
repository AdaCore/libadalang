import os
import sys


sys.path.append(os.path.join(
    os.environ['LIBADALANG_ROOTDIR'],
    '..', 'contrib'
))
import check_same_logic
check_same_logic.main(check_same_logic.parser.parse_args(
    ['foo.adb']))
