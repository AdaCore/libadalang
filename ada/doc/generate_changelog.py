"""
This script will take every commit documenting a public-api change in
libadalang and in langkit, and will extract this doc and concatenate it in a
single file that will be integrated to libadalang's documentation.

The format to document a public-api change is::

    public-api: <subject>
    <body>
    end-public-api

Everything before and after will be removed (even on the same line)
"""

from os import path
import re
import os
from subprocess import check_output

file_path = path.dirname(path.abspath(__file__))

HEADER = """
Changelog for libadalang
========================

This page includes a changelog of every public facing change made in
libadalang, in chronologic order. It is useful if you're using a recent version
of libadalang, and you want to be aware of potential breaking API changes or
new features.

"""

# Set the directory to the script directory, where the documentation also lives
os.chdir(file_path)

langkit_path = path.join(file_path, "..", "..", "langkit")

command = ['git', 'log', '--grep', 'public-api',
           "--format='format:%s%nDate: %ai%n%b'"]

logs = check_output(command, cwd=file_path) + check_output(command, cwd=langkit_path)

commits = [map(str.strip, t) for t in re.findall(
    r".*?public-api: (.*?)\n"
    r"Date: (\d\d\d\d-\d\d-\d\d).*?\n"
    r"(.*?)end-public-api", logs, flags=re.DOTALL
)]

with open("changelog.rst", "w") as f:
    f.write(HEADER)
    for title, date, blob in commits:
        f.write(title)
        f.write("\n" + "-" * len(title) + "\n\n")
        f.write("Changed on " + date + "\n\n")
        f.write(blob)
        f.write('\n\n')
