------------------------------------------------------------------------------
--                        STATIC STACK ANALYSIS TOOL                        --
--                                                                          --
--                      D I S P A T C H I N G _ C A L L S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2010-2011, AdaCore                     --
--                                                                          --
-- GNATstack  is free  software;  you can redistribute  it and/or modify it --
-- under terms of the  GNU General Public License as published  by the Free --
-- Software  Foundation;  either version 3,  or (at your option)  any later --
-- version.  GNATstack is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANT- --
-- ABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General Public License distributed with GNATstack; see file COPYING3. If --
-- not,  go to  http://www.gnu.org/licenses  for a  complete  copy  of  the --
-- license.                                                                 --
------------------------------------------------------------------------------

with Global_Data;
with Static_Stack_Analysis; use Static_Stack_Analysis;

package body Dispatching_Calls is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_Overriding_Methods
     (Node : Dispatching_Node) return List_Of_Edges.Vector;
   --  This function returns the list of potential targets for a dispatching
   --  call. The dispatching call is specified using the index in a vtable
   --  used for dispatch, the type whose vtable is used, and the static type
   --  of the expression.

   procedure Propagate_Derived_Classes
     (Position : Static_Stack_Analysis.List_Of_Classes.Cursor);
   --  Include in the traversed classes the list of descendent

   procedure Propagate_Overloaded_Methods
     (Position : Static_Stack_Analysis.List_Of_Virtual_Methods.Cursor);
   --  Include in the traversed methods the list of methods which overload them

   ------------------------------
   -- Create_Class_Hierarchies --
   ------------------------------

   procedure Create_Class_Hierarchies is
   begin
      --  Iterate over all the classes

      Static_Stack_Analysis.List_Of_Classes.Iterate
        (Global_Data.List_Of_Classes, Propagate_Derived_Classes'Access);
   end Create_Class_Hierarchies;

   -------------------------------
   -- Create_Method_Hierarchies --
   -------------------------------

   procedure Create_Method_Hierarchies is
   begin
      --  Iterate over all the methods

      Static_Stack_Analysis.List_Of_Virtual_Methods.Iterate
        (Global_Data.List_Of_Methods, Propagate_Overloaded_Methods'Access);
   end Create_Method_Hierarchies;

   ----------------------------
   -- Get_Overriding_Methods --
   ----------------------------

   function Get_Overriding_Methods
     (Node : Dispatching_Node) return List_Of_Edges.Vector
   is
      Result : List_Of_Edges.Vector := List_Of_Edges.Empty_Vector;

      Base_Method : constant Virtual_Class_Access :=
        Get_Method (Vtable_Loc'(Node.Base_Class, Node.Slot));

      Method_Subprogram : Node_Id;

      Static_Class : Class_Access renames Node.Static_Class;

      procedure Add_To_Potential_Targets
        (Position : Static_Stack_Analysis.List_Of_Virtual_Methods.Cursor);

      procedure Add_To_Potential_Targets
        (Position : Static_Stack_Analysis.List_Of_Virtual_Methods.Cursor)
      is
         Explored_Method : constant Virtual_Class_Access :=
           List_Of_Virtual_Methods.Element (Position);

      begin
         if
           Static_Class.Derived.Contains (Explored_Method.Vtable_Entry.Class)
              and then
           Explored_Method /= null
              and then
           Explored_Method.Symbol_Name /= null
         then
            --  The method is overloaded in a class which is a descencent
            --  of the static class used for dispatching, so it is a potential
            --  canditate for dispatching and we add it to the list.
            --
            --  If we do not have information about the subprogram associated
            --  to the method, it means that the method is abstract, so it
            --  does not count for dispatching.

            Method_Subprogram := Get_Node (Explored_Method.Symbol_Name.all);

            if List_Of_Nodes.Has_Element (Method_Subprogram) then

               --  Mark the potential target of the dispatching call as
               --  reachable.

               List_Of_Nodes.Element (Method_Subprogram).Is_Called := True;

               Static_Stack_Analysis.List_Of_Edges.Append
                 (Result,
                  new Static_Stack_Analysis.Edge_Element'(
                    Ada_File_Name  => Node.Ada_File_Name,
                    Target         => Method_Subprogram,
                    Point_Of_Call  => Node.Location_List,
                    Feasible_Call  => True,
                    Under_Analysis => False));
            end if;
         end if;
      end Add_To_Potential_Targets;

   --  Start of processing for Get_Overriding_Methods
   begin
      --  We need to get the list of methods which override the one defined
      --  in the base class, and from this set we take only those which are
      --  defined by classes derived from the static class.

      --  At least there is the base method

      --  Get the subprogram associated to the base method. If it is not in
      --  the list, it means that the method is abstract, so it does not count
      --  for dispatching.

      if Base_Method /= null and then Base_Method.Symbol_Name /= null then

         --  Get the demangled name from the symbol name

         Method_Subprogram := Get_Node (Base_Method.Symbol_Name.all);

         if List_Of_Nodes.Has_Element (Method_Subprogram) then
            --  Mark the potential target of the dispatching call as reachable

            List_Of_Nodes.Element (Method_Subprogram).Is_Called := True;

            Static_Stack_Analysis.List_Of_Edges.Append
              (Result,
               new Static_Stack_Analysis.Edge_Element'(
                 Ada_File_Name  => Node.Ada_File_Name,
                 Target         => Method_Subprogram,
                 Point_Of_Call  => Node.Location_List,
                 Feasible_Call  => True,
                 Under_Analysis => False));
         end if;
      end if;

      --  Now we need to iterate over the list of overriding methods.
      --
      --  When Base_Method is null it means that GNATstack does not have any
      --  information about this method, so the only thing that we can say is
      --  that it is an unresolved call.

      if Base_Method /= null then
         --  We need to use the Redefined_By list that comes from either
         --  the Base_Method or the one in the first base class.
         --  The C++ compiler, when a method is overridden, generates a
         --  pointer to the root of this method (the one defined by the
         --  first base class). The Ada compiler generates a pointer to the
         --  method directly overloaded.
         --  For Example, if we have the hierarchy Parent->Child->Grandchild
         --  and the three classes define a method called Foo, the Ada
         --  compiler generates the following for Grandchild::Foo:
         --  "1:grandchild__foo,1:child__foo"
         --  while the C++ compiler generates the following:
         --  "1:grandchild__foo,1:parent__foo"
         --
         --  Note that there is another issue to consider for Ada. If we have
         --  again the hierarchy Parent->Child->Grandchild, and Parent and
         --  Grandchild (but no Child) define a method called Foo, the Ada
         --  compiler generates the following for Grandchild::Foo:
         --  "1:grandchild__foo,1:child__foo"
         --  even when the method Child::Foo does not actually exist. The C++
         --  compiler consistently generates the following:
         --  "1:grandchild__foo,1:parent__foo"
         --  This issue is handled at the moment of propagating the different
         --  methods upstream (in Propagate_Overloaded_Methods), when we add
         --  to every method the list of methods are overloading it, putting
         --  the information in the Redefined_By field.

         declare
            Overriding_List : List_Of_Virtual_Methods.Set :=
              List_Of_Virtual_Methods.Empty_Set;

            First_Base_Method : Virtual_Class_Access;

         begin
            if List_Of_Virtual_Methods.Is_Empty (Base_Method.Redefined_By) then
               --  If the list of methods overloading the direct base is empty
               --  it means that we may need to verify the method in the
               --  first base method. The reason for this is the different
               --  information generated by Ada and C++. When using Ada, the
               --  Redefined_By field is update throughout the whole class
               --  hierarchy, while in C++ this information is propagated
               --  only to the first base classes.
               --
               --  Note that here, there may be two different first base
               --  classes (the one in field Redefines and the one in field
               --  Redefines_Multiple). We can simply take the field Redefines
               --  (which is always available) because the required
               --  information has been propagated to both.

               First_Base_Method := Get_Method (Base_Method.Redefines);

               if First_Base_Method /= null then
                  Overriding_List := First_Base_Method.Redefined_By;
               end if;

            else
               Overriding_List := Base_Method.Redefined_By;
            end if;

            List_Of_Virtual_Methods.Iterate
              (Overriding_List, Add_To_Potential_Targets'Access);
         end;
      end if;

      return Result;
   end Get_Overriding_Methods;

   -------------------------------
   -- Propagate_Derived_Classes --
   -------------------------------

   procedure Propagate_Derived_Classes
     (Position : Static_Stack_Analysis.List_Of_Classes.Cursor)
   is
      Accumulated_Descendent : Static_Stack_Analysis.List_Of_Classes.Set :=
        Static_Stack_Analysis.List_Of_Classes.Empty_Set;

      Child : constant Class_Access := List_Of_Classes.Element (Position);

      procedure Propagate_To_Parent
        (Position : Static_Stack_Analysis.List_Of_Classes.Cursor);

      procedure Propagate_To_Parent
        (Position : Static_Stack_Analysis.List_Of_Classes.Cursor)
      is
         Parent : constant Class_Access := List_Of_Classes.Element (Position);
      begin
         --  Include the list of descendent classes to the parent

         Parent.Derived.Union (Accumulated_Descendent);

         --  Add this class to the list and continue going upstream

         Accumulated_Descendent.Insert (Parent);

         Static_Stack_Analysis.List_Of_Classes.Iterate
           (Parent.Parents, Propagate_To_Parent'Access);

         --  Remove me from the list

         Accumulated_Descendent.Delete (Parent);
      end Propagate_To_Parent;

   --  Start of processing for Propagate_Derived_Classes
   begin
      --  Add this class to the list of classes to propagate upstream

      Accumulated_Descendent.Insert (Child);

      --  Go up the parent's links

      Static_Stack_Analysis.List_Of_Classes.Iterate
        (Child.Parents, Propagate_To_Parent'Access);
   end Propagate_Derived_Classes;

   ----------------------------------
   -- Propagate_Overloaded_Methods --
   ----------------------------------

   procedure Propagate_Overloaded_Methods
     (Position : Static_Stack_Analysis.List_Of_Virtual_Methods.Cursor)
   is
      Accumulated_Overloaded :
      Static_Stack_Analysis.List_Of_Virtual_Methods.Set :=
        Static_Stack_Analysis.List_Of_Virtual_Methods.Empty_Set;

      Overloaded : constant Virtual_Class_Access :=
        List_Of_Virtual_Methods.Element (Position);

      procedure Propagate_To_Base (Base : Vtable_Loc);

      procedure Propagate_To_Base (Base : Vtable_Loc) is
         Overloaded_Method : Virtual_Class_Access;
         Primary_Parent    : Class_Access;

      begin
         if Base = Null_Vtable_Loc then
            --  Stop propagating because this is not overriding any other
            --  method.

            null;

         else
            Overloaded_Method := Static_Stack_Analysis.Get_Method (Base);

            if Overloaded_Method = null then
               --  It means that this method is overriding another one, but the
               --  one to which it is pointing is not a real one and we need to
               --  look for the real on going upstream in the class hierarchy.
               --
               --  This happens with the Ada compiler when we have  hierarchy
               --  like Parent->Child->Grandchild, and Parent and Grandchild
               --  (but no Child) define a method called Foo. The Ada compiler
               --  generates the following for Grandchild::Foo:
               --  "1:grandchild__foo,1:child__foo"
               --  even when the method Child::Foo does not actually exist. The
               --  C++ compiler consistently generates the following:
               --  "1:grandchild__foo,1:parent__foo"

               --  ??? Go up through the primary parents hierarchy. Note that
               --  we may need to go up through the secondary parents also.
               --  This should be fixed in the compiler.

               Primary_Parent := Base.Class.Primary_Parent;

               while Primary_Parent /= null loop
                  Overloaded_Method := Static_Stack_Analysis.Get_Method
                    (Vtable_Loc'(Primary_Parent, Base.Slot));

                  if Overloaded_Method = null then
                     Primary_Parent := Primary_Parent.Primary_Parent;
                  else
                     --  Found

                     exit;
                  end if;
               end loop;

            end if;

            if Overloaded_Method /= null then
               --  Include the list of overloaded methods to the base

               Overloaded_Method.Redefined_By.Union (Accumulated_Overloaded);

               --  Add this method to the list and continue going upstream
               --  through both the primary and secondary vtables that may be
               --  overridden.

               Accumulated_Overloaded.Insert (Overloaded_Method);

               --  Go up the bases' links (primary and secondary)

               Propagate_To_Base (Overloaded_Method.Redefines);

               Propagate_To_Base (Overloaded_Method.Redefines_Multiple);

               --  Remove me from the list

               Accumulated_Overloaded.Delete (Overloaded_Method);
            end if;
         end if;
      end Propagate_To_Base;

   --  Start of processing of Propagate_Overloaded_Methods
   begin
      --  Add this method to the list of methods to propagate upstream

      Accumulated_Overloaded.Insert (Overloaded);

      --  Go up the bases' links (primary and secondary)

      Propagate_To_Base (Overloaded.Redefines);

      Propagate_To_Base (Overloaded.Redefines_Multiple);
   end Propagate_Overloaded_Methods;

   -------------------------------
   -- Resolve_Dispatching_Calls --
   -------------------------------

   procedure Resolve_Dispatching_Calls is
      Index : List_Of_Nodes.Cursor :=
        Global_Data.List_Of_Subprograms.First;

      Node : Static_Stack_Analysis.Node_Class_Access;

   begin
      --  Loop for every instance of dispatching call

      while Static_Stack_Analysis.List_Of_Nodes.Has_Element (Index) loop

         Node := Static_Stack_Analysis.List_Of_Nodes.Element (Index);

         if Node.all in Dispatching_Node'Class then
            --  If the node is an artificial dispatching node, we need to
            --  verify whether we have all the potential targets. If the
            --  information is not there yet we compute and store it.

            List_Of_Edges.Append
              (Node.Target_Calls,
               Dispatching_Calls.Get_Overriding_Methods
                 (Dispatching_Node (Node.all)));

         end if;

         Index := Static_Stack_Analysis.List_Of_Nodes.Next (Index);
      end loop;
   end Resolve_Dispatching_Calls;

end Dispatching_Calls;
