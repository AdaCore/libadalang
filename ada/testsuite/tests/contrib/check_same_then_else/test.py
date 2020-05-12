import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_same_then_else


check_same_then_else.main(check_same_then_else.parser.parse_args(
    ['backend-be_corba_ada-common.adb',
     'backend-be_corba_ada-helpers_internals.adb',
     'be-be_messages.adb',
     'be-obj_ids-factory.adb',
     'be-pvp-pvp_pass-prop-propagate_top_down.adb',
     'disassemblers.ads',
     'exp_aggr.adb',
     'exp_attr.adb',
     'exp_util.adb',
     'foo.adb',
     'gnatdoc-frontend-comment_parser.adb',
     'help_module.adb',
     'language-ada.adb',
     'soap-generator-skel.adb',
     'traces_elf.adb']))
