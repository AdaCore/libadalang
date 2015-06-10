import sys
from os import path


def setenv():
    """
    Sets the python environment so that we have access to the code generator
    """
    current_dir = path.dirname(path.abspath(__file__))
    sys.path.extend([path.join(current_dir, '../langkit')])
