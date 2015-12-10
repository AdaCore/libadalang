#! /usr/bin/env python

from glob import glob
from os import mkdir
from os.path import exists, join, dirname, realpath
import shutil
import subprocess
import sys
import xml.etree.ElementTree as ET


libadalang_dir = dirname(dirname(realpath(__file__)))
diags_dir = join(libadalang_dir, 'pycharm_diagnostics_out')

if exists(diags_dir):
    shutil.rmtree(diags_dir)
mkdir(diags_dir)

try:
    p = subprocess.Popen(
        [
            'inspect.sh',
            libadalang_dir,
            join(libadalang_dir, 'inspectionProfiles', 'Project_Default.xml'),
            diags_dir,
            '-d', join(libadalang_dir, 'ada', 'language'),
            '-d', join(libadalang_dir, 'langkit', 'langkit')
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE
    )
except OSError:
    sys.exit('You need to put the binary path of pycharm in your PATH')

stdout, stderr = p.communicate()
if p.returncode != 0:
    print >> sys.stderr, 'pycharm returned an error ({}):'.format(p.returncode)
    print stdout
    print stderr
    sys.exit(1)

for xml_file in glob(join(diags_dir, '*.xml')):
    tree = ET.parse(xml_file)
    for problem in tree.getroot():
        file_text = problem[0].text[21:]
        line_text = problem[1].text
        problem_text = problem[5].text
        print '{}:{} - {}'.format(file_text, line_text, problem_text)

shutil.rmtree(diags_dir)
