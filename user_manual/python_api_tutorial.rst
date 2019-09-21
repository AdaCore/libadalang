Python API Tutorial
###################

Now that you are familiar with Libadalang's :ref:`core-concepts`, let's
actually do some practice with the Python API.

.. note:: For the moment, only Python 2.7 is supported. We plan to add support
    for Python 3 in the near future - see
    https://github.com/AdaCore/libadalang/issues/325 for status.

Preliminary setup
=================

As seen in the section on core concepts, the first thing to do in order to use
Libadalang is to create an analysis context. We'll create a simple ``test.py``
file with the following content:

.. code-block:: python

   import libadalang as lal
   context = lal.AnalysisContext()


This very simple program will allow us to make sure the Python environment is
properly setup to use Libadalang. Running the above file should yield no error
and no result.

.. code-block:: shell

   # Empty program output
   $ python test.py
   $

Browse the tree
===============

Ok, so now let's do something useful with Libadalang. Let's create a program
that will read all the source files given in argument and then output all the
object declarations they contain.

Once you have an analysis context at hand, parsing an existing source file into
an analysis unit is very simple:

.. code-block:: python

   import libadalang as lal
   context = lal.AnalysisContext()
   unit = context.get_from_file("my_ada_file.adb")

Assuming that parsing went well enough for the parsers to create a tree,
:meth:`libadalang.AnalysisUnit.root` will return the root node associated to
``unit``. You can then use :meth:`libadalang.AdaNode.finditer` on the root node
to iterate on every node in the tree via a generator:

.. code-block:: python

    import libadalang as lal
    context = lal.AnalysisContext()
    unit = context.get_from_file("my_ada_file.adb")

    for node in unit.root.finditer(lambda n: True):
        pass

If there are fatal parsing errors, or if the file cannot be read, the unit
root will be null, but the unit will have diagnostics that you can access via
the :meth:`libadalang.AnalysisUnit.diagnostics` property on the analysis unit.
The property will return a list of :class:`libadalang.Diagnostic`.

.. code-block:: python

    if unit.diagnostics:
        for d in unit.diagnostics:
            print("{}: {}".format(d.sloc_range.start, d.message))

Now what can we do with a node? One of the first things to do is to check its
type: is it a subprogram specification? a call expression? an object
declaration? The way to do that in Python is by calling the
:meth:`libadalang.AdaNode.is_a` method on a node, giving a type object as a
parameter (it's just a shortcut for ``isinstance``). Here, we want to
specifically process the nodes whose type is :class:`libadalang.ObjectDecl`.

Another useful thing to do with nodes is to relate them to the original source
code. The first obvious way to do this is to get the source code excerpts that
were parsed to create them: :meth:`libadalang.AdaNode.text` does this. Another
way is to get the source location corresponding to the first/last tokens that
belong to this node: :meth:`libadalang.AdaNode.sloc_range` will do this,
returning a :class:`libadalang.SlocRange`. This provides the expected start/end
line/column numbers.

.. code-block:: python

   print("Line {}: {}".format(node.sloc_range.start.line, repr(node.text)))

Put all these bit in the right order, and you should get something similar to
the following program:

.. code-block:: python

    import sys
    import libadalang as lal

    context = lal.AnalysisContext()

    for filename in sys.argv[1:]:
        unit = context.get_from_file(filename)
        print("== {} ==".format(filename))
        for d in unit.diagnostics:
            print("{}: {}".format(filename, d))

        if unit.root:
            for node in unit.root.finditer(lambda n: n.is_a(lal.ObjectDecl)):
                print("Line {}: {}".format(
                    node.sloc_range.start.line, repr(node.text)))

If you run this program on the :ref:`ada example program <ada example
program>`, you should get:

.. code-block:: text

   == main.adb ==
   Line 33: u'Context : constant LAL.Analysis_Context := LAL.Create_Context;'
   Line 38: u'Filename : constant String := Ada.Command_Line.Argument (I);'
   Line 39: u'Unit     : constant LAL.Analysis_Unit :=\n            Context.Get_From_File (Filename);'

Follow references
=================

While the previous section only showed Libadalang's syntactic capabilities, we
can go further with semantic analysis. The most used feature in this domain is
the computation of cross references ("xrefs"): the ability to reach the
definition a particular identifier references.

Resolving files
---------------

As mentioned in the :ref:`core-concepts` section, the nature of semantic
analysis requires to know how to fetch compilation units: which source file and
where? Teaching Libadalang how to do this is done through the use of :ref:`unit
providers <unit-providers>`.

The default unit provider, i.e. the one that is used if you don't pass anything
specific to :class:`libadalang.AnalysisContext`, assumes that all
compilation units follow the `GNAT naming convention
<http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#file-naming-rules>`_
and that all source files are in the current directory.

If the organization of your project is not so simple, you have two options
currently in Python:

* You can use features from the auto-provider, provided by
  :meth:`libadalang.UnitProvider.auto` to let Libadalang automatically discover
  your source files.

* You can use features from the project provider, provided by
  :meth:`libadalang.UnitProvider.for_project` to use a GNAT project file.

Be aware though, that because of lack of access to proper Python API to process
GNAT project files, the corresponding facilities in Python are limited for the
moment. If the above options are not sufficient for you, we recommend using the
:ref:`Ada API <Ada API Tutorial>`.

In our program, we'll create a simple project unit provider if a project file
is provided. If not, we'll use the default settings.

Finally, let's update our code to use Libadalang's name resolution
capabilities: when we find an object declaration, we'll print the entity
representing the type of the object declaration.

.. code-block:: python
    :linenos:
    :emphasize-lines: 26

    import libadalang as lal
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('--project', '-P', type=str)
    parser.add_argument('files', help='Files to analyze', type=str, nargs='+')
    args = parser.parse_args()

    provider = None
    if args.project:
        provider = lal.UnitProvider.for_project(args.project)

    context = lal.AnalysisContext(unit_provider=provider)

    for filename in args.files:
        unit = context.get_from_file(filename)
        print("== {} ==".format(filename))
        for d in unit.diagnostics:
            print("{}: {}".format(filename, d))

        if unit.root:
            for node in unit.root.finditer(lambda n: n.is_a(lal.ObjectDecl)):
                print("Line {}: {}".format(
                    node.sloc_range.start.line, repr(node.text)
                ))
                type_decl = node.f_type_expr.p_designated_type_decl
                if type_decl:
                    print("   type is: {}".format(repr(type_decl.text)))

The most interesting line is emphasized above and does the following:

* The access to the field :meth:`libadalang.ObjectDecl.f_type_expr` will get
  the node representing the type expression, which is the type reference for
  the declared object. The result is a node of type :class:`libadalang.TypeExpr`.

* Then, we call :meth:`libadalang.TypeExpr.p_designated_type_decl`, which will
  fetch the type declaration corresponding to this type expression, of type
  :class:`libadalang.BaseTypeDecl`.

This time, running this updated program on the 
:ref:`equivalent Ada version <resolving types>` will yield something like:

.. code-block:: text

   == main.adb ==
   Line 33: u'Context : constant LAL.Analysis_Context := LAL.Create_Context;'
      type is: u'type Analysis_Context is tagged private;'
   Line 38: u'Filename : constant String := Ada.Command_Line.Argument (I);'
      type is: u'type String is array (Positive range <>) of Character;'
   Line 39: u'Unit     : constant LAL.Analysis_Unit :=\n            Context.Get_From_File (Filename);'
      type is: u'type Analysis_Unit is tagged private;'

We have seen here :meth:`libadalang.TypeExpr.p_designated_type_decl`, which
resolves references to types, but Libadalang offers many more properties to
deal with name resolution in Ada:

* :meth:`libadalang.AdaNode.p_xref` will try to resolve from any node to the
  corresponding declaration, much like an IDE would do when you Control-click
  on an identifier, for instance.

* All the ``p_body_part*`` and ``p_decl_part*`` properties will let you
  navigate between the specification and body that correspond to each other for
  various nodes: subprograms, packages, etc.

* :meth:`libadalang.AdaNode.p_expression_type` returns the type of an expression.

* :meth:`libadalang.AdaNode.p_generic_instantiations` returns the list of
  package/subprogram generic instantiations that led to the creation of this
  node.

You can find these and all the other properties documented in your favorite
language's API reference.
