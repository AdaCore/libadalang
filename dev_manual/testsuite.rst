##################
Testing Libadalang
##################

This section deals with how to run and write testcases for Libadalang.

Firstly, the current testsuite is based on the new GNATpython's testsuite
framework. At the user level, on one side there's the engine that you run and
on the other side there are all the testcases to run. The engine's entry point
is the ``testsuite/testsuite.py`` script and in the ``testsuite/tests``
directory you will find all testcases organized in subdirectories: each
testcase is a directory that contains a ``test.yaml`` file.


*********************
Running the testsuite
*********************

Unlike the build of Libadalang itself, the testsuite framework requires Python
3.8 or later versions as well as the `e3-testsuite
<https://github.com/AdaCore/e3-testsuite/>`_ Python library.

The quickest and easiest way to get the work done is running using the ``test``
command from ``manage.py``:

.. code-block:: shell

    python manage.py test

This is actually a wrapper around the "real" testsuite framework. The need for
such a wrapper arises from the fact that you have to properly setup your
environment before running the "real" testsuite entry point.  Indeed, as
regular Libadalang users, the testcases need to have regular access to the
generated libraries, binaries, and so on:

* ``PATH`` must point to the ``bin`` subdirectory in your build prefix;
* ``LD_LIBRARY_PATH`` must point the the ``lib`` subdirectory
* etc.

Note that ``manage.py`` provides a helper to ease the setup. Assuming you have
a Bourne shell, the following makes your environment ready to run the
testsuite:

.. code-block:: shell

    eval `python manage.py setenv`

Once this is done, you can execute:

.. code-block:: shell

    python testsuite/testsuite.py

This will run all the testcases and then output a summary (number of
success/failures). If you want to run only a subset of tests, for instance only
testcases for the C API, run:

.. code-block:: shell

    python testsuite/testsuite.py tests/c_api

If you want to run tests under Valgrind, add the ``--valgrind`` option:

.. code-block:: shell

    python testsuite/testsuite.py --valgrind

It is possible to run tests that check the Libadalang Python bindings using a
different Python interpreter. For instance, to check them against Python 3.7,
run:

.. code-block:: shell

    python manage.py test --with-python=python3.7

To know more about available options, use the ``--help`` option.

.. code-block:: shell

    testsuite/testsuite.py --help

As a final note, you can keep using the ``ada/manage.py test`` command and
still pass additional options to the testsuite:

.. code-block:: shell

    ./manage.py test --valgrind


Python testcases
================

If fate leads you to troubleshoot Libadalang's Python API, you may prefer to
use a debuggable Python interpreter.  Once you built one, say in
``/usr/debug/``, you can run the testsuite using it with the ``--with-python``
testsuite option:

.. code-block:: shell

    ada/manage.py test -- --with-python=/usr/debug/bin/python


*****************
Writing testcases
*****************

As said above, all directories under ``testsuite/tests`` that contain a
``test.yaml`` file are processed as testcases. These are YAML descriptions for
your testcases and the only mandatory field for these is ``driver``, which
describes how to run your testcase.

The Libadalang testsuite defines several test drivers, because there are
different ways to test features. For instance the ``parser`` driver is
specialized into checking that Libadalang gets the correct AST out of some
source and the appropriate parsing rule while the ``python`` driver lets you
run a Python script and check its output so that you can easily test the Python
API.

So when you want to write a new test, think about what you want to test
specifically and then choose the most appropriate driver to write your testcase.
Note that at some point you may want to write your own test driver to write a
lot of testcases that all do the same kind of things.

If you want to see example of how test drivers are used, you can search for
existing testcases that depend on them. For instance, to look for testcases
that use the ``python`` driver, run:

.. code-block:: shell

    grep -R 'driver: python' testsuite/tests

Common knowledge
================

Whatever test driver you use, you have to provide a ``test.out`` text file that
contains the "expected output" for you testcase. Each test driver has its own
meaning for "expected output".

If, for various reasons, you consider it is temporarily acceptable for your
testcase not to provide the expected output, you can add an ``expect_failure``
key to the ``test.yaml`` file. This key must contain either ``null`` or better:
a string that says why the failure is expected, provide a related ticket, etc.
Then, when the testcase will fail, it will be annotated as "XFAIL" in test
results. If it succeeds, that will be "UOK" (Unexpected OK).

Note that tests that are expected to fail are annotated as "FAILED" anyway when
the reason for the failure is not an unexpected output, but instead something
like a build failure, or some mandatory file not found.

C API driver
============

Use the C API driver (``c-api``) as soon as you want your testcase to run C
code that uses Libadalang's C API. For this driver, the ``test.yaml`` requires:

* ``compile_units`` key to hold the list of the C source files that needs to be
  compiled;

* ``input_sources`` key that contains the list of the Ada source files that
  your testcase uses.

Then create an ``test.out`` file that contains the output your C program is
supposed to have if you testcase was successful. The testsuite framework will
display the difference if it does not get the same during runs.

As a good practice, you should use output rather than assertion to check
things: if for one reason or another your program stops correctly before
expected, this anomaly will not show up in the output. As a convention,
testcases currently print a ``Done!`` line just before returning from the main
to make sure this specific issue does not happen.

Parser driver
=============

Use this driver (``parser``) when you want to check the AST for some source
code. This driver also makes it possible to test source-location-based lookups
on the AST and to test the indentation engine.

To use it, first write a ``input`` text file that contains the Ada source to
parse. Then in the ``test.yaml``:

* add a ``rule`` key (mandatory) that contains the name of the parsing rule to
  run on the input.

* the ``action`` key controls what is tested:

    * ``pretty-print`` (by default if the ``action`` key is missing) prints the
      AST in a specific format (all nodes with all their fields);
    * ``indent`` prints the unindented code and then the re-indented one using
      the indentation engine.

* if you want to test the source-location-based lookups, add a ``lookups`` key
  that contains a list such as:

  .. code-block:: yaml

      lookups:
        - {line: 8, column: 13}
        - {line: 10, column: 5}

  After the global, pretty-print, this will trigger lookup for each provided
  sloc and will pretty-print the resulting node on the test output.

Python API driver
=================

The Python API driver (``python``) is very similar to the C API one except that
it works for Python scripts to test the Python API. For this driver, the
``test.yaml`` only requires the ``input_sources`` key to hold the list of the
Ada source files that you testcase uses.

Then create a ``test.py`` Python script: it will be the entry point invoked by
the driver. The output of this script will then be compared to the content of
the ``test.out`` file.

Just like for the C API driver, it is considered a good practice to print a
``Done!`` line just before exiting the script.
