import os
import sys


sys.path.append(os.path.join(
    os.environ['LIBADALANG_ROOTDIR']
))
import stylechecks
stylechecks.main()
