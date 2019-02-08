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
* Install every Python dependency. We recommend creating a virtualenv and
  installing them inside of it, this way:

  .. code-block:: sh

     $ virtualenv env
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

    $ python ada/manage.py build

Assuming you satisfied all the above dependencies, both commands should
successfuly run to completion.

While developing Libadalang you might be happy to use the following command:

.. code-block:: sh

   $ python ada/manage.py make

It will wrap the two previous commands in one, generating the code and building
it in one step.


Install
-------

Once you built Libadalang, you can install the library in any place you want:

.. code-block:: sh

   $ python ada/manage.py install $INSTALL_DIR


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


Static libraries
----------------

By default, the ``ada/manage.py`` script only deals with shared libraries. This
is the most sensible default as using Libadalang's Python bindings requires
shared libraries. In order to also build static ones, just insert the
``--enable-static`` argument to all commands. For instance:

.. code-block:: sh

   $  python ada/manage.py --enable-static generate
   $  python ada/manage.py --enable-static build
   $  python ada/manage.py --enable-static install $INSTALL_DIR

The above will generate, build and then install both the shared libraries and
the static ones.


Building the documentation
--------------------------

Libadalang itself is required to build this Sphinx documentation: this allows
to automatically generate the Ada API reference from the corresponding Ada
source code (conversely for Python). As a consequence, you need either to have
Libadalang installed or to update your environment without installing it: see
the corresponding section above.

Building this documentation as a set of static HTML pages is as easy as
running the following command from the ``user_manual`` directory:

.. code-block:: sh

   $ make html

Assuming successful completion, the documentation is then available in
the ``user_manual/_build/html`` directory: you can start reading it from the
``index.html`` page.

Note that on Mac OS X, security features require you to explicitly pass the
``LD_LIBRARY_PATH`` environment variable:

.. code-block:: sh

   $ make html LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
