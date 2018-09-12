Ada API Tutorial
################

Now that you are familiar with Libadalang's :ref:`core-concepts`, let's
actually do some practice with the Ada API.

Preliminary setup
=================

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

If there were fatal parsing, or if the file cannot be read, the unit root will
be null, but the unit will have diagnostics: see the
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

Another useful thing to do with nodes is to relate them to the original source
code. The first obvious way to do this is to get the source code excerpts that
were parsed to create them: the ``Libadalang.Analysis.Text`` node primitive
does this. Another way is to get the source location corresponding to the
first/last tokens that belong to this node: the
``Libadalang.Analysis.Sloc_Range`` node primitive will do this, returning a
``Langkit_Support.Slocs.Source_Location_Range`` record. This provides the
expected start/end line/column numbers.

.. code-block:: ada

   with Langkit_Support.Slocs;
   package Slocs renames Langkit_Support.Slocs;

   Put_Line
     ("Line"
      & Slocs.Line_Number'Image (Node.Sloc_Range.Start_Line)
      & ": " & Node.Text);

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
               & ": " & Node.Text);
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
