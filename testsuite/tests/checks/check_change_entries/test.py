import os
import subprocess
import sys

from utils import LAL_ROOTDIR


subprocess.check_call([
    sys.executable,
    os.path.join(LAL_ROOTDIR, 'user_manual', 'changes', 'process_changes.py'),
    'rst',
    '--quiet'
])
