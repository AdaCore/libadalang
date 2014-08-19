import sys
from os import path

current_dir = path.dirname(path.abspath(__file__))
sys.path.extend([path.join(current_dir, '../ada')])
