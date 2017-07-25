from __future__ import absolute_import, division, print_function

import os

from utils import get_ext_src, run_nameres


# This testcase assumes that GNATCOLL has been configured in-tree, i.e. that
# src/gnatcoll.gpr exists.
gnatcoll_dir = os.path.join(get_ext_src('gnatcoll'), 'src')

run_nameres(['--quiet', '--files-from-project',
             '-P{}'.format(os.path.join(gnatcoll_dir, 'gnatcoll.gpr'))])
