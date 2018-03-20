from __future__ import absolute_import, division, print_function

import os

from utils import get_ext_src, run_nameres


gnatcov_dir = get_ext_src('gnatcoverage')

# This testcase assumes that GNATCOLL has been configured in-tree, i.e. that
# src/gnatcoll.gpr exists.
gnatcoll_dir = os.path.join(get_ext_src('gnatcoll'), 'src')

os.environ['GPR_PROJECT_PATH'] = gnatcoll_dir
run_nameres(['--only-show-failures', '--files-from-project', '--all',
             '-P{}'.format(os.path.join(gnatcov_dir, 'gnatcov.gpr')),
             '-XBINUTILS_SRC_DIR=/doesnotexists',
             '-XBINUTILS_BUILD_DIR=/doesnotexists'])
