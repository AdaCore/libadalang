"""
Test that Lkt's typer resolves the libadalang spec entirely.
"""
import os
import subprocess


nodes_lkt_path = os.path.join(
    os.environ['LIBADALANG_ROOTDIR'],
    'ada',
    'nodes.lkt'
)
subprocess.check_call(['lkt_toolbox', '-C', nodes_lkt_path])
print("Done.")
