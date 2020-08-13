.. _core-concepts:

Core concepts
#############

Regardless of which language you use in order to write your programs, the
Libadalang API comes with several core concepts. Understanding these upfront
will make your programming experience easier, so the goal of this section is to
make you familiar with them.

Analysis contexts and units
===========================

In order to use Libadalang, the first thing you need to do is to create an
analysis context. These are holders that embed all data and computations done
during source code analysis. The second step is to create what we call an
analysis unit: a holder for computations with respect to a single source file.

Analysis units really map to source files: Libadalang will process one source
file that contains several compilation units as one analysis unit; similarly a
subunit ("separate") gets a dedicated unit.

In order to create an analysis unit, you just need to have a context at hand
and provide either a source file or a buffer that contains the source text to
be analyzed. This will decode the source text (from ``UTF-8``, ``ISO-8859-1``,
etc.), run the lexer on it to get a stream of tokens, and then run the parser
to get the parsing tree. Note that creating an analysis unit always succeeds:
if there are parsing errors or if there is an error opening the file, a
analysis unit is returned nevertheless, with error messages stored in a
diagnostics vector.

While you can create analysis units explicitly using the analysis context API,
semantic analysis also create them on demand. For instance, trying to resolve
``Ada.Containers.Hash_Type`` will automatically load an analysis unit for the
``Ada.Containers`` runtime unit in order to fetch and return the node
corresponding to the ``Hash_Type`` type declaration.

Tokens and trivias
==================

While traditional lexers only give you access to regular tokens and discard
information such as white spaces and comments, Libadalang can preserve all
formatting information. This option, enabled by default when creating analysis
contexts, makes Libadalang suitable for a lot of usages beyond regular
compiler-style analysis.

In order to achieve this, Libadalang keeps track of two distinct sequences of
"lexemes": regular tokens and trivias. The sequence of regular tokens
(identifiers, keywords, etc.) is the one that the parser will use to create its
parsing tree, while the sequence of trivias is just ignored by parsers, but
still preserved for later analysis.

Although Libadalang exposes both sequences as a single one (ranges of tokens
for the analysis units or for single nodes), getting the index of a regular
token will return the position of this token in the list of regular tokens, and
conversely for trivias. You can see this in the following example:

.. code-block:: ada

   procedure Foo;  --  Comment

This source will yield 3 tokens and 3 trivias:

1. the ``procedure`` keyword (token index 1),
2. the first white space trivia (trivia index 1),
3. the ``Foo`` identifier (token index 2),
4. the ``;`` token (token index 3),
5. the second white space trivia (trivia index 2),
6. the comment trivia (trivia index 3).

Depending on the context, "token" designates either only regular tokens or both
regular tokens and trivias.

.. todo:: Maybe we should remove this oddity and expose a single index for both
   tokens and trivias...

Syntax and semantic tree
========================

After a successful parsing step, an analysis unit holds the resulting parsing
tree. Each node in this tree has several characteristics:

* a specific kind (identifier, object declaration, binary expression, ...),
* a first token and a last one (we derive source location and source text
  excerpts from these).

Nodes can be grouped by kind in three categories:

Regular nodes
   These have a definite number of children. For instance, binary operations
   always have three children: the left operand, the operator and the right
   operand. Some children can be null, for example the default expression in a
   parameter specification node.

List nodes
   These can have any number of children (including zero).

Token nodes
   They encompass exactly one token and have no children. For instance: string
   literals.

Sometimes, a node is created to mean the absence of some keywords. For example
the ``Mode_Default`` node is created to describe the absence of explicit mode
for parameter specification. We call these "ghost" nodes.

Children for all nodes can be accessed by index, but only the children of
regular nodes get named accessors. For instance, parameter specification
nodes have a ``mode`` field as their third children; this field is exposed as
the ``F_Mode`` function in the Ada API and the ``.f_mode`` attribute in the
Python API.

The syntax tree also serves for semantic analysis. Look for functions whose
names match ``P_*`` in the Ada API or ``.p_*`` node members in the Python API:
these are what we call node *properties* (hence the P prefix). They compute
semantic information on top of the syntax tree. The most useful example is the
``P_Xref`` property, which computes cross-reference information.  On legal Ada
code bases, properties are always supposed to return a correct result. However,
on incorrect code, they can either return approximate results, or raise
``Property_Error`` exceptions (``PropertyError`` in Python).

In interactive tools such as IDEs, the source code can evolve so that it is
necessary to recompute information. In Libadalang, the analysis unit creation
primitives can reload an analysis unit if the corresponding source file was
already analyzed before. As a result, the previous syntax tree is released and
replaced with the new one: any attempt to use references to released nodes will
raise a ``Stale_Reference_Error`` exception (``StaleReferenceError`` in
Python).

.. _unit-providers:

Unit providers
==============

Some Ada constructs such as ``with`` clauses require name resolution in
Libadalang to "jump" from one source file to another. As an example, processing
the ``with Ada.Text_IO;`` statement requires to load the source files that
contain the ``Ada`` package specification and the ``Ada.Text_IO`` one.

Depending on projects, knowing which source file to use in order to find a
compilation unit can be arbitrarily complex. This is why Libadalang comes with
the concept of unit provider: given a unit name (``Ada.Strings.Unbounded``) and
a unit kind (specification/body), a unit provider must either return a source
file to read, or raise an error if the unit name is not valid.

The analysis context constructor takes an unit provider: name resolution will
use this provider whenever it needs to resolve a unit reference to a source
file. In the absence of an explicit unit provider, the default one will look
for source files in the current directory following the GNAT naming convention:
for instance ``parent-child.ads`` for the specification of the ``Parent.Child``
unit.

Libadalang allows you to create your own unit provider, should the naming
convention of your project be totally custom, but it also comes with two very
useful providers:

* one that takes a GPR project file and gives access to all Ada sources
  referenced by the corresponding project tree (the "project unit provider"),

* one that looks for all source files that match a given file name pattern in a
  given list of directory (the "auto unit provider").

.. _unit-providers-aggregate:

Aggregate projects
------------------

Due to the very nature of Libadalang's operations regarding name resolution, it
expects exactly one source file for each unit name/kind. This assumption
allows the "get to the definition" operation to return exactly one result. The
`aggregate project
<https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug/gnat_project_manager.html#aggregate-projects>`_
formalism is lax and allows concurrent declarations and implementations to
coexist in a single project tree, which violates the assumption described in
the previous paragraph.

For instance, consider the following aggregate project:

.. code-block:: ada

   --  "agg.gpr"

   aggregate project Agg is
      for Project_Files use ("main32.gpr", "main64.gpr");
   end Agg;


The following sources (two alternative implementations of the same package):

.. code-block:: ada

   --  "arch32/arch.ads"
   package Arch is
      type Target_Address is mod 2 ** 32;
   end Arch;

   --  "arch64/arch.ads"
   package Arch is
      type Target_Address is mod 2 ** 64;
   end Arch;

And finally the following alternative main projects:

.. code-block:: ada

   --  "main32.gpr"
   project Main32 is
      for Source_Dirs use (".", "arch32");
      for Main use ("main.adb");
      for Object_Dir use "obj/main32";
   end Main32;

   --  "main64.gpr"
   project Main64 is
      for Source_Dirs use (".", "arch64");
      for Main use ("main.adb");
      for Object_Dir use "obj/main64";
   end Main64;

   --  "main.adb"
   with Ada.Text_IO, Arch;
   procedure Main is
   begin
      Ada.Text_IO.Put_Line
        ("Arch.Target_Address'Size =" & Arch.Target_Address'Size'Image);
   end;

Building the aggregate project with ``gprbuild -Pagg -p -j8 -q`` yields two
alternative ``main`` executables:

.. code-block:: sh

   $ obj/main32/main
   Arch.Target_Address'Size = 32
   $ obj/main64/main
   Arch.Target_Address'Size = 64

Just like the output of the "main" program depends on which ``Arch`` package is
used, the result of the "get to the definition" query on
``Arch.Target_Address`` above depends on it.

Because of this, Libadalang's project provider has multiple ways to work, each
one coming with its own restrictions on the aggregate projects passed to it:

1. When only passed an aggregate project, that project must contain at most one
   source file per unit name/kind. It is not the case in the example above,
   because there are two files (``arch32/arch.ads`` and ``arch64/arch.ads``)
   associated to the spec of the ``Arch`` unit.

2. When passed an aggregate project plus a reference to a specific project file
   in the whole project tree, the unit provider will consider only the source
   files that this project file and all its dependencies own contain.  In the
   example above, this means that one could give the ``agg.gpr`` project file
   plus a reference to the ``main32.gpr`` project: Libadalang would then
   only analyze the ``arch32/arch.ads`` source file.

3. Libadalang can also create several project providers, each one having a
   restricted view on the sub-project it has access to, so that only one source
   file is available for each unit name/kind.

While the two first options are available in all Libadalang APIs, option 3. is
currently available only in its Ada API.

See the :ref:`examples_aggregate_projects` section for related code examples.
