.. _ada api tutorial:

Ada API tutorial
################

Now that you are familiar with Libadalang's :ref:`core-concepts`, let's
actually do some practice with the Ada API.

Preliminary setup
=================

.. attention:: This whole section is going to show you how to create a
    Libadalang app from scratch. This is good, and walks you over the concepts
    about how to use Libadalang. However, if all you want is a command line
    generic application, you should consider using the :ref:`ada-generic-app`.

As the previous section says, the first thing to do in order to use Libadalang
is to create an analysis context:

.. code-block:: ada

   with Libadalang.Analysis;

   procedure Main is
      package LAL renames Libadalang.Analysis;

      Context : constant LAL.Analysis_Context := LAL.Create_Context;
   begin
      null;
   end Main;

This very simple program will allow us to make sure the build environment is
properly setup to use Libadalang. Save the above in a ``main.adb`` source file,
and then write the following project file to ``lal_test.gpr``:

.. code-block:: ada

   with "libadalang";

   project LAL_Test is
      for Main use ("main.adb");
      for Object_Dir use "obj";
   end LAL_Test;

Now you can run GPRbuild to compile the test program:

.. code-block:: shell

   # Build with debug information
   $ gprbuild -Plal_test.gpr -p -g

This command should return without error and create an executable in
``obj/main`` or ``obj\main.exe`` depending on your platform. The last step is
to check if the program works properly: it should do nothing, so no errors
expected!

.. code-block:: shell

   # Empty program output
   $ obj/main
   $

Browse the tree
===============

Ok, so now let's do something actually useful with Libadalang. Let's create a
program that will read all the source files given in argument and then output
all the object declarations they contain.

Once you have an analysis context at hand, parsing an existing source file into
an analysis unit is very simple:

.. code-block:: ada

   Context : constant LAL.Analysis_Context := LAL.Create_Context;
   Unit    : constant LAL.Analysis_Unit :=
      Context.Get_From_File ("my_file.adb");

Both ``Analysis_Context`` and ``Analysis_Unit`` values are references
(assignment creates an alias, not a copy), and these are reference-counted, so
you don't need to do anything special to do regarding resource allocation.

Assuming that parsing went well enough for the parsers to create a tree, the
``Libadalang.Analysis.Root`` unit primitive will return the root node
associated to ``Unit``. You can then use the ``Libadalang.Analysis.Traverse``
node primitive to call a function on all nodes in this tree:

.. code-block:: ada

   function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
   --  Process the given node and return how to continue tree traversal

   Unit.Root.Traverse (Process_Node'Access);

If there are fatal parsing errors, or if the file cannot be read, the unit
root will be null, but the unit will have diagnostics: see the
``Libadalang.Analysis.Has_Diagnostics``, ``Diagnostics`` and
``Format_GNU_Diagnostic`` unit primitives to check the presence of diagnostics,
get their list, and format them into user-friendly error messages.

.. code-block:: ada

   --  Report parsing errors, if any
   if Unit.Has_Diagnostics then
      for D of Unit.Diagnostics loop
         Put_Line (Unit.Format_GNU_Diagnostic (D));
      end loop;
   end if;

Now what can we do with a node? One of the first things to do is to check the
kind: is it a subprogram specification? a call expression? an object
declaration? The ``Libadalang.Analysis.Kind`` node primitives will tell,
returning the appropriate value from the
``Libadalang.Common.Ada_Node_Kind_Type`` enumeration. Here, we want to process
specifically the nodes whose kind is ``Ada_Object_Decl``.

.. attention::
    There is a correspondence between kind names and type names: The kind is
    prefixed by the language name, so the type name for an object declaration
    is ``Object_Decl``, and the kind name is ``Ada_Object_Decl``.

    For abstract node types with several derived types, such as ``Basic_Decl``,
    subtypes are exposed with the corresponding name and range (here
    ``Ada_Basic_Decl``).

Another useful thing to do with nodes is to relate them to the original source
code. The first obvious way to do this is to get the source code excerpts that
were parsed to create them: the ``Libadalang.Analysis.Text`` node
primitive does this. Another way is to get the source location corresponding to
the first/last tokens that belong to this node: the
``Libadalang.Analysis.Sloc_Range`` node primitive will do this, returning a
``Langkit_Support.Slocs.Source_Location_Range`` record. This provides the
expected start/end line/column numbers.

.. code-block:: ada

   with Langkit_Support.Slocs;
   with Langkit_Support.Text;

   package Slocs renames Langkit_Support.Slocs;
   package Text renames Langkit_Support.Text;

   Put_Line
     ("Line"
      & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
      & ": " & Text.Image (Node.Text));

Accessing node fields
---------------------

Another thing to do with nodes is to access their fields. Each kind of node has
a specific set of fields: child nodes in the parsing tree. For instance,
``Object_Decl`` nodes have 8 syntactic fields:

* ``F_Ids``: identifiers for the declared objects;
* ``F_Has_Aliased``: node to materialize the presence/absence for the
  ``aliased`` keyword;
* ``F_Has_Constant``: node to materialize the presence/absence for the
  ``constant`` keyword;
* ``F_Mode``: node to materialize the parameter passing mode (when the object
  declaration is used as a generic formal);
* ``F_Type_Expr``: type for the declared objects;
* ``F_Default_Expr``: expression to initialize the declared objects or provide
  a default value;
* ``F_Renaming_Clause``: part that follows the ``renames`` keyword when the
  declaration is a renaming.
* ``F_Aspects``: list of aspects associated to this declaration.

Accessing them is as simple as using the homonym primitive on the node that
contains the field. For instance, in order to get the type expression for an
object declaration:

.. code-block:: ada

   Obj : Object_Decl;

   Put_Line ("Type expression: " & Obj.F_Type_Expr.Image);

Note that is is always valid to access syntax fields for non-null objects. Some
fields may contain a null node, for instance the ``Object_Decl.F_Default_Expr``
field is null for the ``V : T;`` object declaration.

.. _ada example program:

Final program
-------------

Put all these bit in the right order, and you should get something similar to
the following program:

.. code-block:: ada

   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;
   with Langkit_Support.Slocs;
   with Langkit_Support.Text;
   with Libadalang.Analysis;
   with Libadalang.Common;

   procedure Main is
      package LAL renames Libadalang.Analysis;
      package LALCO renames Libadalang.Common;
      package Slocs renames Langkit_Support.Slocs;
      package Text renames Langkit_Support.Text;

      function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
      --  If Node is an object declaration, print its text. Always continue the
      --  traversal.

      ------------------
      -- Process_Node --
      ------------------

      function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
        use type LALCO.Ada_Node_Kind_Type;
      begin
         if Node.Kind = LALCO.Ada_Object_Decl then
            Put_Line
              ("Line"
               & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
               & ": " & Text.Image (Node.Text));
         end if;
         return LALCO.Into;
      end Process_Node;

      Context : constant LAL.Analysis_Context := LAL.Create_Context;
   begin
      --  Try to parse all source file given as arguments
      for I in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Filename : constant String := Ada.Command_Line.Argument (I);
            Unit     : constant LAL.Analysis_Unit :=
               Context.Get_From_File (Filename);
         begin
            Put_Line ("== " & Filename & " ==");

            --  Report parsing errors, if any
            if Unit.Has_Diagnostics then
               for D of Unit.Diagnostics loop
                  Put_Line (Unit.Format_GNU_Diagnostic (D));
               end loop;

            --  Otherwise, look for object declarations
            else
               Unit.Root.Traverse (Process_Node'Access);
            end if;
            New_Line;
         end;
      end loop;
   end Main;

If you run this program on its own sources, you should get:

.. code-block:: text

   == main.adb ==
   Line 33: Context : constant LAL.Analysis_Context := LAL.Create_Context;
   Line 38: Filename : constant String := Ada.Command_Line.Argument (I);
   Line 39: Unit     : constant LAL.Analysis_Unit :=\x0a            Context.Get_From_File (Filename);

Note on API discoverability
---------------------------

The Ada syntax is rich; as a consequence, there are many node kinds, and each
have many syntax fields. Short of reading the language grammar, the best way to
discover the nodes that parsing creates is to let Libadalang parse an example
and print the resulting tree. This is easily done with the ``Print`` procedure:

.. code-block:: ada

   --  Test program

   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;

   with Libadalang.Analysis; use Libadalang.Analysis;

   procedure Parse is
      Ctx : constant Analysis_Context := Create_Context;
      U   : constant Analysis_Unit :=
        Ctx.Get_From_File (Ada.Command_Line.Argument (1));
   begin
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      U.Root.Print;
   end Parse;

   --  Source to parse

   package Pkg is
   end Pkg;

Running the above program on the ``pkg.ads`` source file yields:

.. code-block:: text

   $ ./parse pkg.ads
   CompilationUnit[1:1-2:9]
   |f_prelude:
   |  AdaNodeList[1:1-1:1]: <empty list>
   |f_body:
   |  LibraryItem[1:1-2:9]
   |  |f_has_private:
   |  |  PrivateAbsent[1:1-1:1]
   |  |f_item:
   |  |  PackageDecl[1:1-2:9]
   |  |  |f_package_name:
   |  |  |  DefiningName[1:9-1:12]
   |  |  |  |f_name:
   |  |  |  |  Id[1:9-1:12]: Pkg
   |  |  |f_aspects: <null>
   |  |  |f_public_part:
   |  |  |  PublicPart[1:15-2:1]
   |  |  |  |f_decls:
   |  |  |  |  AdaNodeList[1:15-1:15]: <empty list>
   |  |  |f_private_part: <null>
   |  |  |f_end_name:
   |  |  |  EndName[2:5-2:8]
   |  |  |  |f_name:
   |  |  |  |  Id[2:5-2:8]: Pkg
   |f_pragmas:
   |  PragmaNodeList[2:9-2:9]: <empty list>

We can see here that the parse tree for ``pkg.ads`` is made of:

* a ``Compilation_Unit`` node as the root of the tree; that node has children
  in 3 syntax fields:
* its ``F_Prelude`` field is an ``Ada_Node_List`` node, that is an empty list
  (i.e. it has no children itself);
* its ``F_Body`` field is a ``Library_Item`` node, which has itself other syntax
  fields (``F_Has_Private`` and ``F_Item``);
* its ``F_Pragmas`` field is a ``Pragma_Node_List`` that is an empty list;
* the ``Package_Decl`` node has a null ``F_Aspects`` syntax field.


Follow references
=================

While the previous section only showed Libadalang's syntactic capabilities, we
can go further with semantic analysis. The most used feature in this domain is
the computation of cross references ("xrefs"): the ability to reach the
definition a particular identifier references.

.. _ada-api-tutorial-unit-provider:

Resolving files
---------------

As mentioned in the :ref:`core-concepts` section, the nature of semantic
analysis requires to know how to fetch compilation units: which source file and
where? Teaching Libadalang how to do this is done through the use of :ref:`unit
providers <unit-providers>`.

The default unit provider, i.e. the one that is used if you don't pass anything
specific to ``Libadalang.Analysis.Create_Context``, assumes that all
compilation units follow the `GNAT naming convention
<http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#file-naming-rules>`_
and that all source files are in the current directory.

If the organization of your project is completely custom, you can either
derive ``Libadalang.Analysis.Unit_Provider_Interface``, implementing the
corresponding primitives according to your project rules, or use features from
the ``Libadalang.Auto_Provider`` package to let Libadalang automatically
discover your source files.

However, if your project can be built with a GPR project file, Libadalang comes
with a LibGPR2 adapter to leverage the knowledge of your GPR files: the
``Libadalang.Project_Provider`` package. Using it should be straightforward for
people familiar with the ``GPR2`` API:

.. code-block:: ada

   declare
      package LAL renames Libadalang.Analysis;
      package LAL_GPR renames Libadalang.Project_Provider;

      Options : GPR2.Options.Object;
      Tree    : GPR2.Project.Tree.Object;

      Context  : LAL.Analysis_Context;
      Provider : LAL.Unit_Provider_Reference;
   begin
      --  Call Options.Add_Switch to add all project-related parameters
      --
      --  Options.Add_Switch (GPR2.Options.P, "my_project.gpr");
      --  ...

      --  Load the project tree, requesting the indexing of all sources,
      --  including runtime units.

      if not Tree.Load
        (Options,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "error loading the project tree";
      end if;

      --  Create a unit provider that uses Tree to resolve references, and an
      --  analysis context using that provider.

      Provider := LAL_GPR.Create_Project_Unit_Provider (Tree);
      Context := LAL.Create_Context (Unit_Provider => Provider);
   end;

Once this compilation unit lookup matter is solved, all you need to do is to
call the right properties to get the job done. Let's update the previous little
program so that it quotes, for each object declaration, the declaration of the
corresponding type. First, use the above code snippet to load a project file
from the first command-line argument:

.. code-block:: ada

   function Load_Project return LAL.Unit_Provider_Reference;
   --  Load the project file designated by the first command-line argument

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project return LAL.Unit_Provider_Reference is
      package LAL_GPR renames Libadalang.Project_Provider;

      Project_Filename : constant String := Ada.Command_Line.Argument (1);
      Options          : GPR2.Options.Object;
      Tree             : GPR2.Project.Tree.Object;
   begin
      Options.Add_Switch (GPR2.Options.P, Project_Filename);
      if not Tree.Load
        (Options,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "error loading the project tree";
      end if;
      return LAL_GPR.Create_Project_Unit_Provider (Tree);
   end Load_Project;

This assumes that the first command-line argument is the name of the project
file to load, so it is necessary to update the iteration on source file
arguments to start at argument number 2:

.. code-block:: ada

   --  Try to parse all remaining source file given as arguments
   for I in 2 .. Ada.Command_Line.Argument_Count loop

Then use our new ``Load_Project`` function when creating the analysis context:

.. code-block:: ada

   Context : constant LAL.Analysis_Context :=
      LAL.Create_Context (Unit_Provider => Load_Project);

.. _resolving types:

Resolving types
---------------

Finally, let's update the ``Process_Node`` function to use Libadalang's name
resolution capabilities: when we find an object declaration, we'll print the
entity representing the type of the object declaration.

.. code-block:: ada

   function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
     use type LALCO.Ada_Node_Kind_Type;
   begin
      if Node.Kind = LALCO.Ada_Object_Decl then
         Put_Line
           ("Line"
            & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
            & ": " & Text.Image (Node.Text));
         declare
            Type_Decl : constant LAL.Base_Type_Decl :=
               Node.As_Object_Decl.F_Type_Expr.P_Designated_Type_Decl;
         begin
            Put_Line ("   type is: " & Text.Image (Type_Decl.Text));
         end;
      end if;
      return LALCO.Into;
   end Process_Node;

The most interesting part is the call to the ``P_Designated_Type_Decl``
property. Let's decompose it:

* ``Node.As_Object_Decl`` converts the input ``Ada_Node`` object into an
  ``Object_Decl`` one. We can do this safely since we checked its kind right
  before.

* The call to ``F_Type_Expr`` (a primitive that is specific to ``Object_Decl``
  nodes) retrieves its type expression field (the type for the declared
  object). The result is a ``Type_Expr`` node.

* Finally the call to the ``P_Designated_Type_Decl`` property fetches the type
  declaration corresponding to this type expression: a ``Base_Type_Decl`` node.

This time, running this updated program on itself will yield something like:

.. code-block:: text

   == main.adb ==
   Line 35: Type_Decl : constant LAL.Base_Type_Decl :=\x0a               Node.As_Object_Decl.F_Type_Expr.P_Designated_Type_Decl;
      type is: type Base_Type_Decl is new Basic_Decl with private\x0a         with First_Controlling_Parameter\x0a      ;
   Line 54: Project_Filename : constant String := Ada.Command_Line.Argument (1);
      type is: type String is array (Positive range <>) of Character;
   Line 55: Options          : GPR2.Options.Object;
      type is: type Object is tagged private;
   [...]

We have seen here the ``P_Designated_Type_Decl`` property, which resolves
references to types, but Libadalang offers many more properties to deal with
name resolution in Ada:

* ``P_Xref`` property will try to resolve from any node to the corresponding
  declaration, much like an IDE would do when you Control-click on an
  identifier, for instance.

* All the ``P_Body_Part*`` and ``P_Decl_Part*`` properties will let you
  navigate between the specification and body that correspond to each other for
  various nodes: subprograms, packages, etc.

* ``P_Expression_Type`` returns the type of an expression.

* ``P_Generic_Instantiations`` returns the list of package/subprogram generic
  instantiations that led to the creation of this node.

You can find these and all the other properties documented in your favorite
language's API reference.

Find all references
-------------------

Source processing tools often need to look for all references to an entity. For
instance: all references to an object declaration, all types that derive from a
type ``T``, all calls to a subprogram ``P``, etc.

Libadalang provides several properties to answer such queries:
``P_Find_All_References``, ``P_Find_All_Derived_Types``, ``P_Find_All_Calls``,
etc. All these properties have in common that they take as argument the list of
analysis units in which to look for the references. For instance, in order to
look for all the references to the ``V`` object declaration in units
``foo.adb``, ``bar.adb`` and ``foobar.adb``, one may write:

.. code-block:: ada

    declare
       Context    : constant Analysis_Context := ...;
       V          : constant Object_Decl := ...;
       V_First_Id : constant Defining_Name := V.F_Ids.List_Child (1);
       Units      : constant Analysis_Unit_Array :=
         (Context.Get_From_File ("foo.adb"),
          Context.Get_From_File ("bar.adb"),
          Context.Get_From_File ("foobar.adb"));
    begin
       Put_Line ("Looking for references to " & V_First_Id.Image & ":");
       for R of V_First_Id.P_Find_All_References (Units) loop
          Put_Line (Kind (R)'Image & " - " & Ref (R).Image);
       end loop;
    end;

The first step is to get the ``Defining_Name`` node on which to perform the
query: in the ``A, B : Integer`` object declaration, for instance, this allows
one to specifically query all references to ``A``. The second step is to select
the set of units in which to look for references. The last step is to call the
``P_Find_All_References`` property and process its results.

This property returns an array of ``Ref_Result`` values, which contain both:
``Ref`` (a ``Base_Id`` node), which constitutes the reference to the defining
name, and ``Kind`` (a ``Ref_Result_Kind`` enumeration value), which gives more
information about this reference: whether Libadalang successfully managed to
compute this information, whether it had to do error recovery or completely
failed (for instance due to incorrect analyzed source code).

List of sources in a project
----------------------------

Even though ``GNATCOLL.Projects`` provides facilities to get the list of source
files in a project, this operation is so common for Libadalang tools that
Libadalang provides a convenience function to compute such a list:
``Libadalang.Project_Provider.Source_Files``. This is especially useful to
compute the analysis units to pass to the ``P_Find_All_*`` properties
(described in the previous section).

This function takes a project tree (``GNATCOLL.Projects.Project_Tree``) and a
mode to determine the scope of the sources to consider (root project only,
the whole project tree, the runtime, ...) and just returns the list of source
files:

.. code-block:: ada

   declare
      Project : Project_Tree := ...;
      Context : Analysis_Context := ...;
      Id      : Defining_Name := ...;
      Sources : constant Filename_Vectors.Vector := Source_Files (Project);
      Units   : Analysis_Unit_Array (1 .. Sources.Last_Index);
   begin
      for I in Units'Range loop
         Units (I) := Context.Get_From_File (To_String (Sources (I)));
      end loop;

      Put_Line ("Looking for references to " & Id.Image & ":");
      for R of Id.P_Find_All_References (Units) loop
          Put_Line (Kind (R)'Image & " - " & Ref (R).Image);
      end loop;
    end;

.. _ada-generic-app:

Ada generic application framework
=================================

Basics
------

In order to facilitate the creation of Ada command line applications,
Libadalang ships an ``App`` generic package (in the ``Libadalang.Helpers``
unit), that you can simply instantiate in order to create a command line
application with a lot of common functionality already built-in, so that you
don't have to reinvent it every time.

The way it works is simple: you instantiate it, providing it several callbacks
(see below) and call its ``Run`` procedure in your main. It then handles all
the logistic around your application:

* parsing command-line arguments,
* setting up unit providers,
* creating analysis contexts,
* creating the list of source files to process for you.

Your callbacks are then invoked when appropriate. The main ones are:

* ``App_Setup`` right after command line options are parsed;
* ``Process_Unit`` when processing one source file;
* ``App_Post_Process`` after all source files are processed.

Let's say you want to create a simple application that will flag all the
``goto`` statements in a given closure. Here is what it would look like:

.. code-block:: ada

    --  app.ads

    with Libadalang.Analysis; use Libadalang.Analysis;
    with Libadalang.Helpers;  use Libadalang.Helpers;

    package App is

       procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

       package App is new Libadalang.Helpers.App
         (Name         => "example_app",
          Description  => "Example app. Will flag goto statements",
          Process_Unit => Process_Unit);

    end App;

    --  app.adb

    with Ada.Text_IO; use Ada.Text_IO;
    with Libadalang.Common; use Libadalang.Common;

    package body App is

       procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
          pragma Unreferenced (Context);

          function Visit (Node : Ada_Node'Class) return Visit_Status;

          function Visit (Node : Ada_Node'Class) return Visit_Status is
          begin
             case Node.Kind is
             when Ada_Goto_Stmt =>
                Put_Line ("Found goto stmt: " & Node.Image);
                return Over;
             when others =>
                return Into;
             end case;
          end Visit;
       begin
          Unit.Root.Traverse (Visit'Access);
       end Process_Unit;
    end App;

    --  main.adb

    with App;

    procedure Main is
    begin
       App.App.Run;
    end Main;

Then, running the app on a project is as simple as

.. code:: bash

    # Files are automatically deduced from the project file
    $ ./main -P my_project.gpr

    # Files are passed explicitly. Default project is used
    $ ./main *.adb

    # Analyze file.adb in the context of project.gpr, with scenario variable
    # BUILD_TYPE set to prod.
    $ ./main file.adb -P project.gpr -XBUILD_TYPE=prod


Parallelism
-----------

Even though it is disabled by default, ``App`` has supports for parallelism. If
the generic instantiation passes ``True`` to the ``Enable_Parallelism`` formal,
then your application will be able to process several units at the same time.

Most of Libadalang is not thread safe, so how could this possibly work? When
running the application, pass for instance the ``-j8`` argument to run 8 jobs
in parallel.  Each job will get its own ``Analysis_Context`` instance, so that
each job actually deals with thread-local data, avoiding concurrency issues.

Working with parallel job requires special attention, which is why it is
disabled by default:

* Calls to ``Job_Setup``, ``Process_Unit`` and ``Job_Post_Process`` happen in
  parallel, so access to data that is not local to a thread must be properly
  synchronized. For instance, concurrent calls to ``Ada.Text_IO.Put_Line`` (on
  the same file) must be protected to avoid mixing line content, counters must
  be protected to avoid ABA problems, etc.

* Since each job creates its own ``Analysis_Context`` instance, each job will
  probably parse and run name resolution on the same units (results are not
  shared between contexts). This means that using 8 jobs will not magically
  divide computing time by 8. This also means that in the worst case, using 8
  jobs can consume up to 8 times the memory required to process the same list
  of units without parallelism.
