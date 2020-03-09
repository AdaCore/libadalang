Accessing actuals of generic instantiations
###########################################

A common need for Libadalang clients is to access actuals of generic
instantiations. Here is a small example of how to do that.

Input source
============

.. code-block:: ada

   package Generic_Instantiation is
      generic
         type T is private;
      package Test_Generic is
         A : T;
      end Test_Generic;

      package Inst is new Test_Generic (Integer);
   end Generic_Instantiation;

Sample code
===========

Python
------

.. code-block:: python

   # Getting the fully qualified name of the generic instantiation's parameter

   print unit.root.find(
      lambda n: n.is_a(lal.Identifier) and n.text == 'Integer'
   ).p_referenced_decl().p_fully_qualified_name

   # Getting the fully qualified name of the type of T, from the instantiation

   # First, get the instantiated generic declaration's subtree
   inst = unit.root.find(lal.GenericInstantiation).p_designated_generic_decl

   # Then, find the "A : T" object decl
   obj_decl = inst.find(lal.ObjectDecl)

   # Then, print the type of the object_decl
   print obj_decl.f_type_expr.p_designated_type_decl.p_fully_qualified_name

Ada
---

.. code-block:: ada

    with Libadalang.Common; use Libadalang.Common;
    with Libadalang.Iterators; use Libadalang.Iterators;
    with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

    ...

    --  Getting the fully qualified name of the generic instantiation's
    --  parameter:

    Put_Line
      --  Find the node corresponding to the generic actual.
      (Find_First
         (Unit.Root,
          Kind_Is (Ada_Identifier)
          and Text_Is ("Integer")).As_Identifier
       --  Then, get the declaration referenced by the actual, and print the
       --  fully qualified name of that declaration.
       .P_Referenced_Decl.P_Fully_Qualified_Name);

    --  Getting the fully qualified name of the type of T, from the
    --  instantiation `Inst`:

    declare
       --  First, get the instantiated package declaration
       Inst : constant Basic_Decl :=
         --  First, we get the generic instantiation node
         Find_First
           (Unit.Root,
            Kind_In
              (Ada_Generic_Instantiation'First,
               Ada_Generic_Instantiation'Last))
         .As_Generic_Instantiation

         --  Then, from this node, get the instantiated generic declaration.
         .P_Designated_Generic_Decl;

       --  From the instantiated generic, find the declaration of `A`
       Obj_Decl : constant Object_Decl :=
         Find_First (Inst, Kind_Is (Ada_Object_Decl)).As_Object_Decl;
    begin
       --  Then get the instantiated type of A, and print its fully qualified
       --  name.
       Put_Line
         (Obj_Decl.F_Type_Expr.P_Designated_Type_Decl
          .P_Fully_Qualified_Name);
    end;
