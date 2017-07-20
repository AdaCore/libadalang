[![Build Status](https://travis-ci.org/AdaCore/libadalang.svg?branch=master
)](https://travis-ci.org/AdaCore/libadalang)

Libadalang
==========

Libadalang is a project to build a high performance semantic engine for the Ada
programming language. It is meant to provide a basis to write Ada tooling,
including tools working on potentially changing and incorrect code, such as
IDEs. Its goals encompass, but are not limited to:

* Full support for parsing the Ada 2012 syntax, plus SPARK extensions.
* Error tolerant parsing: the parser must be able to recover from simple errors
  and provide a "best-guess" tree.
* Error tolerant semantic analysis: it must be possible to create a tool that
  works only on syntax, and completely ignores semantic issues.
* Full symbol resolution respecting Ada 2012 – and prior Ada versions –
  semantics.
* Bindings to a variety of languages, including Ada, C, Python and Java, so
  that tools can be written from various ecosystems.
* Incremental processing of source files.

Come and discuss with us on our [public HipChat
room](https://www.hipchat.com/g34Il1Cen)!

High level architecture
-----------------------

Libadalang is a library that can be used from Ada and Python. Most of its code
is Ada code, generated from the language specification that you can find in
[ada/language](ada/language).

*WARNING*: You will *not* find the generated code in the repository. You have
to generate it yourself. We're thinking about some plans to auto-generate the
code and put it in another repo/branch.

It is using the [Langkit](https://github.com/AdaCore/langkit) framework as a
basis, and is at the time of writing the main project developped using it.

The language specification, while embedded in Python syntax, is mostly its own
language, the Langkit DSL, that is used to specify the part of Ada syntax and
semantics that are of interest to us.

Status of the project
---------------------

Libadalang is still in development. its APIs are not stable, the shape of the
abstract syntax tree is not yet completelely stable, and most of its features
are either not stable or not fully implemented.

It is not yet safe to rely on the API stability of Libadalang in your projects.
However, Libadalang is used internally in some AdaCore projects, so you might
find it as a project dependency.

Libadalang currently:

* Is able to parse 100% of Ada 2012 syntax, and presents a well formed tree for
  it.

* Is able to recover some syntax errors, but is still currently behind GNAT in
  that regard.

* Provides some name resolution/navigation. Name resolution is the item on
  which most work is focused as of now. A sizable part of the language is
  handled, but it is not yet complete.

* Is able to handle some very simple incremental processing. Reparsing a source
  A and querying xref on a source B that depends on A is theoretically
  supported, and works in some cases, but the infrastructure is not yet general
  enough.

For those reasons, Libadalang is only suited today for mostly syntactic tools.


Libadalang and ASIS
-------------------

ASIS is widely used for static analysis of Ada code, and is an ISO standard. It
is still the go-to tool if you want to create a tool that analyses Ada code.
Also, as explained above, Libadalang is not mature yet, and cannot replace ASIS
in tools that require semantic analysis.

However, there are a few reasons you might eventually choose to use Libadalang
instead of ASIS:

1. The ASIS standard has not yet been updated to the 2012 version of Ada. More
   generally, the advantages derived from ASIS being a standard also means that
   it will evolve very slowly.

2. Syntax only tools will derive a lot of advantages on being based on
   Libadalang:

   * Libadalang will be completely tolerant to semantic errors. For example, a
     pretty-printer based on Libadalang will work whether your code is
     semantically correct or not, as long as it is syntactically correct.

   * Provided you only need syntax, Libadalang will be much faster than ASIS'
     main implementation (AdaCore's ASIS), because ASIS always does complete
     analysis of the input Ada code.

3. The design of Libadalang's semantic analysis is lazy. It will only process
   semantic information on-demand, for specific portions of the code. It means
   that you can get up-to-date information for a correct portion of the code
   even if the file contains semantic errors.

4. Libadalang has bindings to C and Python, and its design makes it easy to
   bind to new languages.

5. Libadalang is suitable to write tools that work on code that is evolving
   dynamically. It can process code and changes to code incrementally. Thus, it
   is suitable as an engine for an IDE, unlike AdaCore's ASIS implementation.

6. Libadalang is not tied to a particular compiler version. This combined with
   its staged and error tolerant design means that you can use it to detect
   bugs in Ada compilers/tools.


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
pass other arguments to `testsuite.py`. For instance, if you want to run under
a debugger only the `factor_0` test case, execute:

    $ python ada/manage.py test -- -g ada/testsuite/tests/parser/factor_0

Documentation
-------------

The developer and user's documentation for Libadalang is in `ada/doc`. You can
consult it as a text files or you can build it.  For instance, to generate HTML
documents, run from the top directory:

    $ make -C ada/doc html

And then open the following file in your favorite browser:

    ada/doc/_build/html/index.html

Playing
-------

Libadalang has a Python API, for easy prototyping and explorative programming.
It ships with an executable named `playground`, that allows you to analyze Ada
files and play with them in an interactive Python console.

Given the following `main.adb` Ada file:

~~~ada
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
    Put_Line ("Hello World");
end Main;
~~~

You can start the playground on it:

~~~sh
% playground main.adb

--
-- libadalang playground
--

The file(s) passed as argument have been put into the `u` variable, or units if
there are multiple.

Enjoy!

In [1]: print u.root.text
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
begin
    Put_Line ("Hello World");
end Main;

In [2]: print u.root.findall(mdl.CallExpr)
[<CallExpr 5:5-5:29>]

In [3]: print u.root.findall(mdl.CallExpr)[0].text
Put_Line ("Hello World")
~~~

The playground embeds the [IPython](https://ipython.org/) interactive Python
console, so you have a modern interactive programming environment. You can use
tab completion to explore the Libadalang API.
