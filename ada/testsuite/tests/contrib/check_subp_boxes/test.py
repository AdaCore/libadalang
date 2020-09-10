import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_subp_boxes


c = check_subp_boxes.CheckSubpBoxes.run(['--fix', 'test.adb'])

with open("test.adb") as f:
    print(f.read())
