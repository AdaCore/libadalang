import os
import sys


sys.path.append(os.path.join(
    os.environ['LIBADALANG_ROOTDIR'],
    '..', 'contrib'
))
import check_same_args
check_same_args.main(check_same_args.parser.parse_args(
    ['foo.adb']))
