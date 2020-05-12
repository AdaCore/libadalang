import glob
import sys

from utils import in_contrib


sys.path.append(in_contrib())
import detect_copy_paste


detect_copy_paste.main(
    detect_copy_paste.parser.parse_args(["--ignore-ids", "--size-min=10"]
                                        + sorted(glob.glob('*.adb')))
)
