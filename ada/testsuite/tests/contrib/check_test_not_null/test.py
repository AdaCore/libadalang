from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_test_not_null
check_test_not_null.main(check_test_not_null.parser.parse_args(
    ['foo.adb']))
