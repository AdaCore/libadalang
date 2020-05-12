import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_same_test


check_same_test.main(check_same_test.parser.parse_args(
    ['foo.adb',
     'gnatdoc-frontend-comment_parser.adb',
     'po_createref_parse_cmd.adb',
     'sem_attr.adb',
     'sem_ch4.adb',
     'sem_eval.adb',
     'sem_prag.adb',
     'sem_res.adb']))
