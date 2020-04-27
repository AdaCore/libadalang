from __future__ import absolute_import, division, print_function

import os
import sys

from utils import LAL_ROOTDIR


sys.path.append(os.path.join(LAL_ROOTDIR, 'ada'))


import stylechecks


stylechecks.main()
