Libadalang
==========

Libadalang is a project to build a high performance semantic engine for the Ada
programming language. It is meant to provide a basis to write Ada tooling,
including tools working on potentially changing and incorrect code, such as
IDEs. Its goals encompass, but are not limited to:

* Full support for parsing the Ada 2012 syntax, plus SPARK extensions.
* Error tolerant parsing: the parser must be able to recover from simple errors
  and provide a "best-guess" tree.
* Full symbol resolution respecting Ada 2012 – and prior Ada versions –
  semantics.
* Bindings to a variety of languages, including Ada, C, Python and Java, so
  that tools can be written from various ecosystems.

It is using the [Langkit](https://github.com/AdaCore/langkit) framework as a
basis, and is at the time of writing the main project developped using it.

Come and discuss with us on our [public HipChat
room](https://www.hipchat.com/gQfbVZ9qL)!


Quick guide to use Libadalang
-----------------------------

In order to use Libadalang, one has first to generate its code and to build it.
You can read and run manually the steps in the "Setup" section below, or you
can use [our script](install-lal-and-deps.sh) to semi-automate this. After
this, you can either use Libadalang in Ada with the `libadalang.gpr` project
file, or in Python just import the `libadalang` module.


Setup
-----

To generate and build the library itself, you'll need to go through the
following steps:

* Install the GNAT tools and compiler. You can find the GPL version of them
  on <http://libre.adacore.com>
* Build and install the GNATcoll library. You can find its source release on
  <http://libre.adacore.com> or directly on GitHub:
  <https://github.com/AdaCore/gnatcoll> (just make sure you checkout the
  `gpl-2016` branch).
* Install Quex version 0.65.4 -
  <http://downloads.sourceforge.net/project/quex/DOWNLOAD/quex-0.65.4.tar.gz>
  Follow the installation guide in the Quex `README`
* Install every Python dependency. We recommend creating a virtualenv and
  installing them inside of it, this way:

        $ virtualenv env
        $ source env/bin/activate
        $ pip install -r REQUIREMENTS.dev

* Setup Langkit. If you use a virtualenv, get it on
  [Github](https://github.com/AdaCore/langkit/) and run:

        $ python langkit/setup.py install

   Otherwise, either add the `langkit` directory to your `PYTHONPATH` or just
   move it in the root directory of this repository.

To develop comfortably:

* If you want interactive debugging when code is generated, install IPython
* If you want to compute code coverage for the code generator, install
  coverage.py (see `REQUIREMENTS.dev`)
* If you want to check memory issues, the testsuite has an option to track them
  using Valgrind.


Building
--------

First, let's generate code for Libadalang itself. In the top-level directory,
run:

    $ python ada/manage.py generate

You can also pass a `-c` flag in order to get a code coverage report in the
"coverage" directory.

Then, let's build it:

    $ python ada/manage.py build

If you satisfied all the above dependencies and if you did set `QUEX_PATH` and
the quex executable as said in quex's `README`, it should build fine.

Also, while developing Libadalang you might be happy to use the

    $ python ada/manage.py make

command, that will wrap the two previous commands in one, generating the code
and building it in one step


Install
-------

Once you built Libadalang, you can install the library in any place you want:

    $ python ada/manage.py install $INSTALL_DIR


Testing
-------

First, make sure you have the `build/bin` directory in your PATH so the
test cases can access the `parse` program. Then, in the top-level directory,
run:

    $ python testsuite/testsuite.py

If you want to learn more about this test driver's options (for instance to run
tests under Valgrind), add a `-h` flag.

Because adding `build/bin` to the `PATH` is not very convenient,
`ada/manage.py` provides a shortcut to run the testsuite:

    $ python ada/manage.py test

It runs the testsuite with the `--enable-color` option. It is also possible to
pass other arguments to `testsuite.py`. For instance, if you want to run under a
debugger only the `factor_0` test case, execute:

    $ python ada/manage.py test -- -g ada/testsuite/tests/parser/factor_0


Documentation
-------------

The developer and user's documentation for Libadalang is in `ada/doc`. You can
consult it as a text files or you can build it.  For instance, to generate HTML
documents, run from the top directory:

    $ make -C ada/doc html

And then open the following file in your favorite browser:

    ada/doc/_build/html/index.html
