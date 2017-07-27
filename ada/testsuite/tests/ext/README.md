Testcases using external sources
================================

This directory contains tests that depend on external sources. This makes it
possible to test Libadalang on real world sources, as a complement to tests
that work on synthetic (and simplistic) testcases.

By default, these testcases are DEAD (not executed) when running the testsuite.
In order to exercise them, you need to put the expected set sources in the
expected location. First, look for the set of required source repositories in
the `external_sources` key in `test.yaml` files. Then setup the corresponding
set of repositories as indicated below.

`acats`
-------

Put the whole ACATS directory as `ada/testsuite/ext_src/acats`.

`gnatcoll`
----------

Get GNATCOLL (https://github.com/AdaCore/gnatcoll) and put it as
`ada/testsuite/ext_src/gnatcoll`. Then, run its `configure` script to generate
the `gnatcoll/src/gnatcoll.gpr` project file.

Alternatively, if you want to use an installed GNATCOLL (for instance the one
shipped with GNAT Pro), write the path to the corresponding project file in
`ada/testsuite/ext_src/gnatcoll/project_file_path.txt`.

`gnatcoverage`
--------------

Get GNATcoverage (https://github.com/AdaCore/gnatcoverage) and move its
`tools/gnatcov` directory as `ada/testsuite/ext_src/gnatcoverage`.
