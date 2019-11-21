.. _ada api tutorial:

Ada API Tutorial
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
were parsed to create them: the ``Libadalang.Analysis.Debug_Text`` node
primitive does this. Another way is to get the source location corresponding to
the first/last tokens that belong to this node: the
``Libadalang.Analysis.Sloc_Range`` node primitive will do this, returning a
``Langkit_Support.Slocs.Source_Location_Range`` record. This provides the
expected start/end line/column numbers.

.. code-block:: ada

   with Langkit_Support.Slocs;
   package Slocs renames Langkit_Support.Slocs;

   Put_Line
     ("Line"
      & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
      & ": " & Node.Debug_Text);

.. _ada example program:

Final program
-------------

Put all these bit in the right order, and you should get something similar to
the following program:

.. code-block:: ada

   with Ada.Command_Line;
   with Ada.Text_IO; use Ada.Text_IO;
   with Langkit_Support.Slocs;
   with Libadalang.Analysis;
   with Libadalang.Common;

   procedure Main is
      package LAL renames Libadalang.Analysis;
      package LALCO renames Libadalang.Common;
      package Slocs renames Langkit_Support.Slocs;

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
               & ": " & Node.Debug_Text);
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
with a ``GNATCOLL.Projects`` adapter to leverage the knowledge of your GPR
files: the ``Libadalang.Project_Provider`` package. Using it should be
straightforward for people familiar with the ``GNATCOLL.Projects`` API:

.. code-block:: ada

   declare
      package GPR renames GNATCOLL.Projects;
      package LAL renames Libadalang.Analysis;
      package LAL_GPR renames Libadalang.Project_Provider;

      Env     : GPR.Project_Environment_Access;
      Project : constant GPR.Project_Tree_Access :=
         new GPR.Project_Tree;

      Context  : LAL.Analysis_Context;
      Provider : LAL.Unit_Provider_Reference;
   begin
      GPR.Initialize (Env);
      --  Use procedures in GNATCOLL.Projects to set scenario
      --  variables (Change_Environment), to set the target
      --  and the runtime (Set_Target_And_Runtime), etc.

      Project.Load (My_Project_Filename, Env);
      Provider := LAL_GPR.Create_Project_Unit_Provider_Reference
        (Project, Env);
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
      package GPR renames GNATCOLL.Projects;
      package LAL_GPR renames Libadalang.Project_Provider;
      use type GNATCOLL.VFS.Filesystem_String;

      Project_Filename : constant String := Ada.Command_Line.Argument (1);
      Project_File     : constant GNATCOLL.VFS.Virtual_File :=
         GNATCOLL.VFS.Create (+Project_Filename);

      Env     : GPR.Project_Environment_Access;
      Project : constant GPR.Project_Tree_Access := new GPR.Project_Tree;
   begin
      GPR.Initialize (Env);
      Project.Load (Project_File, Env);
      return LAL_GPR.Create_Project_Unit_Provider_Reference
        (Project, Env);
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
            & ": " & Node.Debug_Text);
         declare
            Type_Decl : constant LAL.Base_Type_Decl :=
               Node.As_Object_Decl.F_Type_Expr.P_Designated_Type_Decl;
         begin
            Put_Line ("   type is: " & Type_Decl.Debug_Text);
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
   Line 30: Project_Filename : constant String := Ada.Command_Line.Argument (1);
      type is: type String is array (Positive range <>) of Character;
   Line 31: Project_File     : constant GNATCOLL.VFS.Virtual_File :=\x0a         GNATCOLL.VFS.Create (+Ada.Command_Line.Argument (1));
      type is: type Virtual_File is tagged private;
   Line 34: Env     : GPR.Project_Environment_Access;
      type is: type Project_Environment_Access is access all Project_Environment'Class;

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

.. _ada-generic-app:

Ada generic application framework
=================================

In order to facilitate the creation of Ada command line applications,
we have added an ``App`` generic package, that you can simply
instantiate in order to create a command line application with a lot
of common functionality already built-in, so that you don't have to reinvent it
every time.

The way it works is that it handles all the logic for you. You just pass it one
or two callbacks:

- One callback, ``Process_Unit``, will be called for each new ``Analysis_Unit``
  that the application processes.
- One callback, ``Process_Context``, will be called at the end when every
  ``Analysis_Unit`` has been processed.

Let's say you want to create a simple application that will flag all the
``goto`` statements in a given closure. Here is what it would look like:


.. code-block:: ada

    --  app.ads

    with Libadalang.Analysis; use Libadalang.Analysis;
    with Libadalang.Helpers;

    package App is

       procedure Process_Unit (Unit : Analysis_Unit);

       package App is new Libadalang.Helpers.App
         (Description  => "Example app. Will flag goto statements",
          Process_Unit => Process_Unit);

    end App;

    --  app.adb

    with Ada.Text_IO; use Ada.Text_IO;
    with Libadalang.Common; use Libadalang.Common;

    package body App is

       procedure Process_Unit (Unit : Analysis_Unit) is
          function Visit (Node : Ada_Node'Class) return Visit_Status;

          function Visit (Node : Ada_Node'Class) return Visit_Status is
          begin
             case Node.Kind is
             when Ada_Goto_Stmt =>
                Put_Line ("Found goto stmt: " & Node.Short_Image);
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
    # BUILD_TYPE set to prod
    $ ./main file.adb -P project.gpr -XBUILD_TYPE=prod

