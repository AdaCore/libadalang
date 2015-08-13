#! /usr/bin/env python

"""Setup configuration file for the Langkit framework"""

import os
from distutils.core import setup


ROOT_DIR = os.path.dirname(__file__)

if ROOT_DIR != '':
    os.chdir(ROOT_DIR)

# Run the setup tools
setup(
    name='Langkit',
    version='0.1-dev',
    author='AdaCore',
    author_email='report@adacore.com',
    url='https://www.adacore.com',
    description='A Python framework to generate language parsers',
    requires=['Mako'],
    packages=['langkit'],
    package_data={'langkit': ['support/*']},
    scripts=[os.path.join('scripts', 'create-project.py')]
)
