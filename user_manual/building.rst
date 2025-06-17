Building Libadalang
###################

First, note that if you have access to a pre-built Libadalang package, bundled
with GNAT Pro if you have a GNAT Pro subscription (for community users, we have
a Libadalang crate on `Alire <https://alire.ada.dev/crates/libadalang>`_), then
you do not need to go through the following steps: just follow the instructions
in the ``README`` file that comes with this pre-built package.


Setup
-----

To generate and build the library itself, you'll need to go through the
following steps:

* Install the GNAT tools and compiler. If do not have access to GNAT Pro, you
  can find public editions of the Ada toolchain on your distribution repository
  (as for example `here <https://packages.debian.org/sid/gnat>`_ for Debian) or
  by installing it from `Alire <https://alire.ada.dev/docs/>`_.

* Build and install the GNATcoll library (core, plus Iconv and GMP bindings).
  You can find its source release on `AdaCore's website
  <https://www.adacore.com/download>`_ or directly on GitHub's repositories for
  `gnatcoll-core <https://github.com/AdaCore/gnatcoll-core>`_ and
  `gnatcoll-bindings <https://github.com/AdaCore/gnatcoll-bindings>`_.

* Build and install the AdaSAT library. You can find its sources on its `GitHub
  repository <https://github.com/AdaCore/adasat>`_.

* Install Langkit, build and install ``Langkit_Support``. Get Langkit's sources
  on `GitHub <https://github.com/AdaCore/langkit>`_, making sure you use the
  same branch in both repositories. For instance, if you use Libadalang's
  ``master`` branch, you should use Langkit's ``master`` branch as well. Then,
  from the checkout root directory, run:

  .. code-block:: sh

     # Install the Langkit python package
     $ pip install .

     # Build the "langkit_support.gpr" project
     $ python manage.py build-langkit-support --library-types=static,static-pic,relocatable

     # Install it. Replace $PREFIX below with the directory where you want to
     # install the langkit_support.gpr project.
     $ python manage.py install-langkit-support --library-types=static,static-pic,relocatable $PREFIX

* Install every Python dependency. We recommend creating a `virtual environment
  <https://packaging.python.org/guides/installing-using-pip-and-virtual-environments/>`_
  and installing them inside of it, this way:

  .. code-block:: sh

     $ python -mvenv env
     $ source env/bin/activate
     $ pip install -r requirements-github.txt
     $ pip install -r requirements-pypi.txt

For GNATcoll, AdaSAT and Langkit, make sure to install the version that
corresponds to the version of Libadalang that is built. For instance, build all
``22.1`` branches, or all ``master`` branches. Mixing versions is not
supported.

To develop comfortably:

* If you want interactive debugging when code is generated, install `IPython
  <https://ipython.org>`_.
* If you want to compute code coverage for the code generator, install
  `coverage.py <https://coverage.readthedocs.io/>`_ (see ``REQUIREMENTS.dev``).
* If you want to check memory issues, the testsuite has an option to track them
  using `Valgrind <http://valgrind.org/>`_.

Regarding the version of these dependencies: Libadalang's development branch
(``master`` branch on the Git repository) is built and tested using the
development version of Langkit, of GNATcoll, and of GNATcoll's own
dependencies, and their release cycles are synchronized. This means that in
order to build Libadalang's branch ``X``, you need to install Langkit using the
branch of the same name, and likewise for GNATcoll and its dependencies.


Building the library
--------------------

First, let's generate code for Libadalang itself. In the top-level directory,
run:

.. code-block:: sh

   $ python -m langkit.scripts.lkm generate

This generates Ada, C and Python source code for Libadalang in the ``build``
directory. In order to build this source code into a shared library, run:

.. code-block:: sh

   $ python -m langkit.scripts.lkm build \
     --library-types=static,static-pic,relocatable

Assuming you satisfied all the above dependencies, both commands should
successfully run to completion.

While developing Libadalang you might be happy to use the following command:

.. code-block:: sh

   $ python -m langkit.scripts.lkm make \
     --library-types=static,static-pic,relocatable

It will wrap the two previous commands in one, generating the code and building
it in one step.

If you are interested in shared (``relocatable``) libraries only, you can omit
the ``--library-types`` argument.


Install
-------

Once you built Libadalang, you can install the library in any place you want:

.. code-block:: sh

   $ python -m langkit.scripts.lkm install \
     $INSTALL_DIR --library-types=static,static-pic,relocatable

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

In addition, if GPRbuild is not installed in ``$INSTALL_DIR``, you need to add
``$INSTALL_DIR/share/gpr`` to the ``GPR_PROJECT_PATH`` environment variable in
order for GPRbuild to locate the installed project files, such as
``libadalang.gpr``.


Using Libadalang without installing it
--------------------------------------

During development, it can be useful to update environment variables so that
Libadalang can be used directly after a build, without performing a bona fide
installation. The ``printenv`` command enables one to do that. Assuming a
Bourne-compatible shell, run:

.. code-block:: sh

   $ eval `python -m langkit.scripts.lkm printenv`

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
documentation extraction helpers, as well as ``sphinxcontrib-adadomain`` to
properly generate Sphinx that documents Ada API:

.. code-block:: sh

   $ pip install contrib/laldoc
   $ pip install git+https://github.com/AdaCore/sphinxcontrib-adadomain

From there, building this documentation as a set of static HTML pages is as
easy as running the following command from the ``user_manual`` directory:

.. code-block:: sh

   $ make newhtml

Assuming successful completion, the documentation is then available in
the ``user_manual/_build/html`` directory: you can start reading it from the
``index.html`` page.

Note that on Mac OS X, security features require you to explicitly pass the
``LD_LIBRARY_PATH`` environment variable:

.. code-block:: sh

   $ make newhtml LD_LIBRARY_PATH="$LD_LIBRARY_PATH"
