import os
import sys


sys.path.append(os.path.join(
    os.environ['LIBADALANG_ROOTDIR']
))
import lal_stylechecks
lal_stylechecks.main([])
