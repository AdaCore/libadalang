import subprocess as sp
import os
from os import path
import argparse


class C:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'

def printcol(msg, color):
    print "{0}{1}{2}".format(color, msg, C.ENDC)


parser = argparse.ArgumentParser(description='Execute the libadalang testsuite')
parser.add_argument('-w', '--write', default=[], nargs=2,
                    help='Write a test with name and input string')
parser.add_argument('-r', '--rewrite', dest='rewrite', action='store_true')
parser.set_defaults(rewrite=False)
args = parser.parse_args()

dr_path = path.dirname(path.realpath(__file__))
os.chdir(dr_path)

if args.write:
    rule_name = args.write[0]
    test_input = args.write[1]
    try:
        i = 0
        while os.path.exists("./{0}_{1}".format(rule_name, i)):
            i += 1
        dirname = "{0}_{1}".format(rule_name, i)
        os.mkdir(dirname)

        out = sp.check_output(["../bin/parse", "-r", rule_name, "--input", test_input])

        printcol("Success", C.OKGREEN)
        print "Got out : {0}".format(out)

        with open(path.join(dirname, "input"), "w") as f:
            f.write(rule_name + "\n")
            f.write(test_input)

        with open(path.join(dirname, "expected"), "w") as f:
            f.write(out)

    except Exception, e:
        printcol("Failed writing test for rule {0}".format(rule_name), C.FAIL)
        print e
else:
    num_passed = 0
    num_failed = 0
    num_rewriten = 0
    for cdir, subdirs, files in os.walk("."):
        if cdir == ".":
            continue
        try:
            with open(path.join(cdir, "input")) as f:
                rule_name = f.readline().strip()
                input_text = f.read().strip()
            out = sp.check_output(["../bin/parse", "-r", rule_name, "--input", input_text])
            with open(path.join(cdir, "expected")) as f:
                expected = f.read()

            if out == expected:
                print "{0}Test passed{1} - {2}".format(C.OKGREEN, C.ENDC, cdir[2:])
                num_passed += 1
            else:
                print "{0}Test failed{1} - {2}".format(C.FAIL, C.ENDC, cdir[2:])
                print "OUT : \t\t", out.strip()
                print "EXPECTED : \t", expected.strip()
                num_failed += 1
                if args.rewrite:
                    print "Rewriting test {0}{1}{2}".format(C.HEADER, cdir, C.ENDC)
                    with open(path.join(cdir, "expected"), "w") as f:
                        f.write(out)

        except Exception, e:
            printcol("Error with test {0}".format(cdir), C.FAIL)
            num_failed += 1
            print e

    print "SUMMARY : {0}{1} passed{2}, {3}{4} failed{5} {6}".format(
        C.OKGREEN, num_passed, C.ENDC, C.FAIL, num_failed, C.ENDC,
        ", {0}{1} rewritten{2}".format(C.WARNING, num_rewriten, C.ENDC) if args.rewrite else ""
    )
