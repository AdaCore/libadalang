import sys

from utils import in_contrib


sys.path.append(in_contrib())
import check_useless_assign


check_useless_assign.main(check_useless_assign.parser.parse_args(
    ['ada_semantic_tree.adb',
     'backend-be_corba_ada-aligned.adb',
     'backend-be_corba_ada-common.adb',
     'be-obj_ids-region_information-factory.adb',
     'exp_ch6.adb',
     'flow-control_flow_graph-utility.adb',
     'foo.adb',
     'gpr-err-scanner.adb',
     'gps-kernel-project.adb',
     'par-ch10.adb',
     'sem_ch3.adb',
     'stm32-spi.adb']))
