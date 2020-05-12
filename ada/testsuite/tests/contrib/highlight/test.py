import subprocess

from utils import gprbuild, in_contrib


gprbuild(in_contrib('highlight', 'highlight.gpr'))
subprocess.check_call(
    [in_contrib('highlight', 'bin', 'highlight'), 'example.adb'],
    cwd=in_contrib('highlight')
)
