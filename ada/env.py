from os import path
import sys


def setenv():
    """
    Sets the python environment so that we have access to the code generator.
    """
    current_dir = path.dirname(path.abspath(__file__))
    # TODO: We actually extend the environment so that ada has access to
    # langkit. This is a kludge and should die the day we have a proper python
    # package for langkit.
    sys.path.extend([path.join(current_dir, '../langkit')])
