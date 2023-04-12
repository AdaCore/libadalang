"""
Check that ``GPRProject.default_charset`` works as expected.
"""

import libadalang as lal


for project in ("default.gpr", "utf8.gpr"):
    p = lal.GPRProject(project)
    print(f"{project}: {p.default_charset()}")
print('Done')
