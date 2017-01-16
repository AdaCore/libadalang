from glob import glob
import os

import libadalang as lal

acats_dir = os.path.join(os.environ['LIBADALANG_ROOTDIR'],
                         'testsuite',
                         'acats')

if not os.path.isdir(acats_dir):
    print "FAIL: You need to put the acats repo in ada/testsuite/acats"
else:
    c = lal.AnalysisContext()

    # Find every C test in the ACATS
    for d in glob(os.path.join(acats_dir, 'acats-4', 'tests', 'c*')):
        for root, dirs, files in os.walk(d):
            for filename in files:
                filepath = os.path.join(root, filename)
                if filename.endswith('.a') or filename.endswith('.ada'):

                    # Parse the file
                    try:
                        u = c.get_from_file(filepath)
                    except Exception:
                        print "ERROR in {}:".format(filepath)
                        raise

                    # Check that the parsing generated no diagnostics
                    if u.diagnostics:
                        print "ERROR in {}:".format(filepath)
                        for d in u.diagnostics:
                            print d
