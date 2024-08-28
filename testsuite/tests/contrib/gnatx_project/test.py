import subprocess

from utils import gprbuild, in_contrib


gprbuild(in_contrib('gnatx_project', 'gnatx_prj.gpr'))
subprocess.check_call(
    [in_contrib('gnatx_project', 'bin', 'main'), 'main.adb'],
    cwd=in_contrib('gnatx_project')
)
