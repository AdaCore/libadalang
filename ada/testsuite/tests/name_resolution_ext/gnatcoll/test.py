from __future__ import absolute_import, division, print_function

from utils import get_gnatcoll_project_file, run_nameres


run_nameres(['--quiet', '--files-from-project',
             '-P{}'.format(get_gnatcoll_project_file())])
