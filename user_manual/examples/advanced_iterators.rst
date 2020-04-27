Using advanced iterators
########################

Libadalang users often need to go over all nodes in a unit that match specific
criteria. For instance: go through all number declarations whose expression is
a simple integer literal. The ``Traverse`` functions in ``Libadalang.Analysis``
already make this more handy than implementing the tree traversal using the
generic ``Child`` function on nodes, but it's still cumbersome, as one need to
declare a callback function to process each node.

The advanced iterators API (``Libadalang.Iterators``) provide helpers to make
this even more convenient.


Input source
============

.. code-block:: ada

   package Tunables is
      Bit_Count   : constant := 1337;
      Bytes_Count : constant := (Bit_Count + 7) / 8;
   end Tunables;


Sample code
===========

.. code-block:: ada

   with Ada.Text_IO; use Ada.Text_IO;

   with Libadalang.Analysis;  use Libadalang.Analysis;
   with Libadalang.Common;    use Libadalang.Common;
   with Libadalang.Iterators; use Libadalang.Iterators;

   procedure Foo is
      Unit : constant Analysis_Unit :=
         Create_Context.Get_From_File ("tunables.ads");

      --  Create a predicate that will accept only...
      P : constant Ada_Node_Predicate :=

         --  Number_Decl nodes...
         Kind_Is (Ada_Number_Decl)

         --  Whose F_Expr fields contain...
         and Child_With
           (Number_Decl_F_Expr,

            --  Int_Literal nodes
            Kind_Is (Ada_Int_Literal));

      --  Create an iterator that will yield all nodes under (and including)
      --  Unit.Root that satisfy this predicate.
      It   : Traverse_Iterator'Class := Find (Unit.Root, P);
      Node : Ada_Node;
   begin
      --  Go through all these nodes
      while It.Next (Node) loop
         Put_Line ("Found: " & Node.Short_Image);
      end loop;
   end Foo;


Output
======

.. code-block:: text

   Found: <NumberDecl ["Bit_Count"] tunables.ads:2:4-2:35>
