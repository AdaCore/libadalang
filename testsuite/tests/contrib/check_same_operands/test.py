import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_same_operands


check_same_operands.main(check_same_operands.parser.parse_args(
    ['foo.adb']))
