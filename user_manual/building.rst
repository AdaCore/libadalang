Building Libadalang
###################

Setup
-----

To generate and build the library itself, you'll need to go through the
following steps:

* Install the GNAT tools and compiler. You can find Community Editions on
  `AdaCore's website <https://www.adacore.com/download>`_.
* Build and install the GNATcoll library (core, plus Iconv and GMP bindings).
  You can find its source release on `AdaCore's website
  <https://www.adacore.com/download>`_ or directly on GitHub's repositories for
  `gnatcoll-core <https://github.com/AdaCore/gnatcoll-core>`_ and
  `gnatcoll-bindings <https://github.com/AdaCore/gnatcoll-bindings>`_. Just
  make sure you checkout the ``gpl-20**`` branch corresponding to your GNAT
  Community release.
* Install every Python dependency. We recommend creating a [virtual
  environment](https://packaging.python.org/guides/installing-using-pip-and-virtual-environments/)
  and installing them inside of it, this way:

  .. code-block:: sh

     $ python -mvenv env
     $ source env/bin/activate
     $ pip install -r REQUIREMENTS.dev

To develop comfortably:

* If you want interactive debugging when code is generated, install `IPython
  <https://ipython.org>`_.
* If you want to compute code coverage for the code generator, install
  `coverage.py <https://coverage.readthedocs.io/>`_ (see ``REQUIREMENTS.dev``).
* If you want to check memory issues, the testsuite has an option to track them
  using `Valgrind <http://valgrind.org/>`_.


Building the library
--------------------

First, let's generate code for Libadalang itself. In the top-level directory,
run:

.. code-block:: sh

   $ python ada/manage.py generate

This generates Ada, C and Python source code for Libadalang in the ``build``
directory. In order to build this source code into a shared library, run:

.. code-block:: sh

    $ python ada/manage.py --library-types=static,static-pic,relocatable build

Assuming you satisfied all the above dependencies, both commands should
successfuly run to completion.

While developing Libadalang you might be happy to use the following command:

.. code-block:: sh

   $ python ada/manage.py --library-types=static,static-pic,relocatable make

It will wrap the two previous commands in one, generating the code and building
it in one step.


Install
-------

Once you built Libadalang, you can install the library in any place you want:

.. code-block:: sh

   $ python ada/manage.py --library-types=static,static-pic,relocatable install $INSTALL_DIR

Then, depending on your operating system and your system configuration, you may
need to update environment variables so that programs can load dynamic
libraries:

.. code-block:: sh

   # On most Unix systems:
   export LD_LIBRARY_PATH=$INSTALL_DIR/lib:$LD_LIBRARY_PATH

   # On Windows, either:
   export PATH=$INSTALL_DIR/bin:$PATH
   # ... or:
   set PATH "$INSTALL_DIR\bin;$PATH"

In addition, if GPRbuild is not installed in $INSTALL_DIR, you need to add
``$INSTALL_DIR/share/gpr`` to the ``GPR_PROJECT_PATH`` environment variable in
order for GPRbuild to locate the installed project files, such as
``libadalang.gpr``.


Using Libadalang without installing it
--------------------------------------

During development, it can be useful to update environment variables so that
Libadalang can be used directly after a build, without performing a bona fide
installation. The ``setenv`` command enables one to do that. Assuming a
Bourne-compatible shell, run:

.. code-block:: sh

   $ eval `python ada/manage.py setenv`

After this, you can both build programs that depend on Libadalang using
GPRbuild and run Python interpreter to import the ``libadalang`` module.


Building the documentation
--------------------------

Libadalang itself is required to build this Sphinx documentation: this allows
to automatically generate the Ada API reference from the corresponding Ada
source code (conversely for Python). As a consequence, you need either to have
Libadalang installed (and in particular its Python bindings) or to update your
environment without installing it: see the corresponding section above.

In addition, you need to install the ``laldoc`` Python project, which contains
documentation extraction helpers:

.. code-block:: sh

   pip install contrib/laldoc

From there, building this documentation as a set of static HTML pages is as
easy as running the following command from the ``user_manual`` directory:

.. code-block:: sh

   $ make html

Assuming successful completion, the documentation is then available in
the ``user_manual/_build/html`` directory: you can start reading it from the
``index.html`` page.

Note that on Mac OS X, security features require you to explicitly pass the
``LD_LIBRARY_PATH`` environment variable:

.. code-block:: sh

   $ make html LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
