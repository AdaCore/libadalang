#! /usr/bin/env python

"""Setup configuration file for laldoc."""

from distutils.core import setup


# Run the setup tools
setup(
    name='laldoc',
    version='0.1-dev',
    author='AdaCore',
    author_email='report@adacore.com',
    url='https://www.adacore.com',
    description='Libadalang-based Ada source extractor for documentation',
    packages=['laldoc']
)
