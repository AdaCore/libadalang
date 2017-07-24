from __future__ import absolute_import, division, print_function

import os
import re
import subprocess

from utils import get_ext_src


gnatcov_dir = os.path.join(get_ext_src('gnatcoverage'), 'tools', 'gnatcov')

# This testcase assumes that GNATCOLL has been configured in-tree, i.e. that
# src/gnatcoll.gpr exists.
gnatcoll_dir = os.path.join(get_ext_src('gnatcoll'), 'src')

directory_missing_re = (
    r'.*\.gpr:\d+:\d+: warning:'
    r' \w+ directory ".*" (not found|does not exist)'
)


os.environ['GPR_PROJECT_PATH'] = gnatcoll_dir
for line in subprocess.check_output(
    ['nameres', '--quiet', '--files-from-project',
     '-P{}'.format(os.path.join(gnatcov_dir, 'gnatcov.gpr')),
     '-XBINUTILS_SRC_DIR=/doesnotexists',
     '-XBINUTILS_BUILD_DIR=/doesnotexists'],
    stderr=subprocess.STDOUT
).splitlines():
    line = line.strip()
    if not re.match(directory_missing_re, line):
        print(line)
