import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_bad_unequal
check_bad_unequal.main(check_bad_unequal.parser.parse_args(
    ['foo.adb', 'himoco-blockdiagramcmg.adb']))
