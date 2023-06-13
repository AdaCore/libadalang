Python API tutorial
###################

Now that you are familiar with Libadalang's :ref:`core-concepts`, let's
actually do some practice with the Python API.

.. note:: Libadalang's Python API supports Python 3.9 and 3.10.

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

Accessing node fields
---------------------

Another thing to do with nodes is to access their fields. Each kind of node has
a specific set of fields: child nodes in the parsing tree. For instance,
``ObjectDecl`` nodes have 8 syntactic fields:

* ``f_ids``: identifiers for the declared objects;
* ``f_has_aliased``: node to materialize the presence/absence for the
  ``aliased`` keyword;
* ``f_has_constant``: node to materialize the presence/absence for the
  ``constant`` keyword;
* ``f_mode``: node to materialize the parameter passing mode (when the object
  declaration is used as a generic formal);
* ``f_type_expr``: type for the declared objects;
* ``f_default_expr``: expression to initialize the declared objects or provide
  a default value;
* ``f_renaming_clause``: part that follows the ``renames`` keyword when the
  declaration is a renaming.
* ``f_aspects``: list of aspects associated to this declaration.

Accessing them is as simple as using the homonym attribute on the node that
contains the field. For instance, in order to get the type expression for an
object declaration:

.. code-block:: python

   obj = get_some_object_decl()
   print(obj.f_type_expr)

Note that is is always valid to access syntax fields for non-null objects. Some
fields may contain a null node, for instance the ``ObjectDecl.f_default_expr``
field is null for the ``V : T;`` object declaration.

Final program
-------------

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

Note on API discoverability
---------------------------

The Ada syntax is rich; as a consequence, there are many node kinds, and each
have many syntax fields. Short of reading the language grammar, the best way to
discover the nodes that parsing creates is to let Libadalang parse an example
and print the resulting tree. This is easily done with the ``dump`` method:

.. code-block:: python

   # Test script

   import libadalang as lal
   import sys

   ctx = lal.AnalysisContext()
   u = ctx.get_from_file(sys.argv[1])
   for d in u.diagnostics:
       print(u.format_gnu_diagnostic(d))
   u.root.dump()

.. code-block:: ada

   --  Source to parse

   package Pkg is
   end Pkg;

Running the above program on the ``pkg.ads`` source file yields:

.. code-block:: text

   CompilationUnit pkg.ads:1:1-2:9
   |f_prelude:
   |  AdaNodeList pkg.ads:1:1-1:1
   |f_body:
   |  LibraryItem pkg.ads:1:1-2:9
   |  |f_has_private:
   |  |  PrivateAbsent pkg.ads:1:1-1:1
   |  |f_item:
   |  |  PackageDecl ["Pkg"] pkg.ads:1:1-2:9
   |  |  |f_package_name:
   |  |  |  DefiningName "Pkg" pkg.ads:1:9-1:12
   |  |  |  |f_name:
   |  |  |  |  Id "Pkg" pkg.ads:1:9-1:12: Pkg
   |  |  |f_aspects: None
   |  |  |f_public_part:
   |  |  |  PublicPart pkg.ads:1:15-2:1
   |  |  |  |f_decls:
   |  |  |  |  AdaNodeList pkg.ads:1:15-1:15
   |  |  |f_private_part: None
   |  |  |f_end_name:
   |  |  |  EndName pkg.ads:2:5-2:8
   |  |  |  |f_name:
   |  |  |  |  Id "Pkg" pkg.ads:2:5-2:8: Pkg
   |f_pragmas:
   |  PragmaNodeList pkg.ads:2:9-2:9

We can see here that the parse tree for ``pkg.ads`` is made of:

* a ``CompilationUnit`` node as the root of the tree; that node has children in
  3 syntax fields:
* its ``f_prelude`` field is an ``AdaNodeList`` node, that is an empty list
  (i.e. it has no children itself);
* its ``f_body`` field is a ``LibraryItem`` node, which has itself other syntax
  fields (``f_has_private`` and ``f_item``);
* its ``f_pragmas`` field is a ``PragmaNodeList`` that is an empty list;
* the ``PackageDecl`` node has a null ``f_aspects`` syntax field.


Follow references
=================

While the previous section only showed Libadalang's syntactic capabilities, we
can go further with semantic analysis. The most used feature in this domain is
the computation of cross references ("xrefs"): the ability to reach the
definition a particular identifier references.

.. _python-api-tutorial-unit-provider:

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
  :meth:`libadalang.GPRProject.create_unit_provider` to use a GNAT project
  file.

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
    :emphasize-lines: 27

    import libadalang as lal
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument('--project', '-P', type=str)
    parser.add_argument('files', help='Files to analyze', type=str, nargs='+')
    args = parser.parse_args()

    provider = None
    if args.project:
        project = lal.GPRProject(args.project)
        provider = project.create_unit_provider()

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


Find all references
-------------------

Source processing tools often need to look for all references to an entity. For
instance: all references to an object declaration, all types that derive from a
type ``T``, all calls to a subprogram ``P``, etc.

Libadalang provides several properties to answer such queries:
``p_find_all_references``, ``p_find_all_derived_types``, ``p_find_all_calls``,
etc. All these properties have in common that they take as argument the list of
analysis units in which to look for the references. For instance, in order to
look for all the references to the ``v`` object declaration in units
``foo.adb``, ``bar.adb`` and ``foobar.adb``, one may write:

.. code-block:: python

   import libadalang as lal

   context: lal.AnalysisContext = ...
   v: lal.ObjectDecl = ...

   v_first_id = v.f_ids[0]
   units = [context.get_from_file("foo.adb"),
           context.get_from_file("bar.adb"),
           context.get_from_file("foobar.adb")]

   print(f"Looking for references to {v_first_id}:")
   for r in v_first_id.p_find_all_references(units):
       print(f"{r.kind}: {r.ref}")

The first step is to get the ``defining_name`` node on which to perform the
query: in the ``A, B : Integer`` object declaration, for instance, this allows
one to specifically query all references to ``A``. The second step is to select
the set of units in which to look for references. The last step is to call the
``p_find_all_references`` property and process its results.

This property returns an array of ``RefResult`` values, which contain both:
``ref`` (a ``BaseId`` node), which constitutes the reference to the defining
name, and ``kind`` (a ``RefResultKind`` enumeration value), which gives more
information about this reference: whether Libadalang successfully managed to
compute this information, whether it had to do error recovery or completely
failed (for instance due to incorrect analyzed source code).

List of sources in a project
----------------------------

Even though there is no dedicated Python API to analyze GNAT project files,
Libadalang provides a convenience function to compute such a list:
``libadalang.GPRProject.source_files``. This is especially useful to compute
the analysis units to pass to the ``p_find_all_*`` properties (described in the
previous section).

This function takes the information necessary to load a project tree (name of
the project file, scenario variables, etc.), a mode to determine the scope of
the sources to consider (root project only, the whole project tree, the
runtime, ...) and just returns the list of source files:

.. code-block:: python

   import libadalang as lal

   project = lal.GPRProject(...)
   context: lal.AnalysisContext = lal.AnalysisContext(
       unit_provider=project.create_unit_provider(...),
       ...
   )
   id: lal.DefiningName = ...

   source_files = project.source_files()
   units = [context.get_from_file(f) for f in source_files]

   print(f"Looking for references to {id}:")
   for r in id.p_find_all_references(units):
       print(f"{r.kind}: {r.ref}")
