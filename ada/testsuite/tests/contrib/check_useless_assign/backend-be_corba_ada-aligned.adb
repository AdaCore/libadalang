------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--         B A C K E N D . B E _ C O R B A _ A D A . A L I G N E D          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2013, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Namet;  use Namet;

with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;
with Backend.BE_CORBA_Ada.Common;      use Backend.BE_CORBA_Ada.Common;

package body Backend.BE_CORBA_Ada.Aligned is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;
   package BEN renames Backend.BE_CORBA_Ada.Nodes;

   package body Package_Spec is

      --  The Args_Type_Out is unusable for the moment, it will be
      --  used just for bounded type

      function Args_Type_Record_In (E : Node_Id) return Node_Id;
      function Args_Type_Record_Out (E : Node_Id) return Node_Id;
      function Args_Type_Access_Out (E : Node_Id) return Node_Id;

      procedure Visit_Specification (E : Node_Id);
      procedure Visit_Module (E : Node_Id);
      procedure Visit_Operation_Declaration (E : Node_Id);
      procedure Visit_Interface_Declaration (E : Node_Id);
      procedure Visit_Structure_Type (E : Node_Id);
      procedure Visit_Union_Type (E : Node_Id);
      procedure Visit_Type_Declaration (E : Node_Id);

      function Make_Variable_Type
        (N         : Node_Id;
         Type_Spec : Node_Id;
         Desc      : List_Id)
        return Node_Id;

      function Is_Unbounded_Type (N : Node_Id) return Boolean;
      --  Return true if N (type spec) contains an unbounded type

      procedure Get_Discriminants
        (N      : Node_Id;
         L      : List_Id;
         Ret    : Boolean := False;
         Struct : Boolean := False);
      --  Fill 'L' with all discriminants of the type 'N'. Ret
      --  indicate if the type is used for return argument in this
      --  case the argument name is 'Return'. Struct indicate if the
      --  type is a structure member.

      -------------------------
      -- Args_Type_Record_In --
      -------------------------

      function Args_Type_Record_In (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         Param      : constant List_Id := Parameters (E);
         Discr      : constant List_Id := New_List;
         Components : constant List_Id := New_List;
         L          : List_Id := No_List;
         Args_Type  : Node_Id := No_Node;
         Component  : Node_Id;
         Par_Type   : Node_Id;
         N          : Node_Id;
         Par        : Node_Id;
      begin
         --  For each subprogram we generate a record containing the
         --  In parameters with the aligned type.

         if not FEU.Is_Empty (Param) then

            Par := First_Entity (Param);

            while Present (Par) loop

               if Is_In (FEN.Parameter_Mode (Par)) then
                  --  If the parameter type is a class-wide type, we
                  --  remove the "'Class" attribute from the type
                  --  name.

                  Par_Type := Type_Spec (Par);

                  if BEN.Kind (Par_Type) = K_Attribute_Reference then
                     Par_Type := Prefix (Par_Type);
                  end if;

                  Par_Type := Make_Type_Designator (Par_Type);

                  if Is_Unbounded_Type (Type_Spec (Par)) then
                     L := New_List;
                     Get_Discriminants (Par, L);
                  end if;

                  Par_Type := Make_Variable_Type
                    (Par_Type, Type_Spec (Par), L);

                  --  Add the discriminants of Par to Discr

                  if not Is_Empty (L) then
                     N := First_Node (L);
                     Append_To (Discr, N);
                     L := No_List;
                  end if;

                  Component := Make_Component_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (IDL_Name (Identifier (Declarator (Par)))),
                     Subtype_Indication  => Par_Type);
                  Append_To (Components, Component);
               end if;

               Par := Next_Entity (Par);
            end loop;
         end if;

         --  Record name

         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_In");

         N := Make_Selected_Component
            (Defining_Identifier (Aligned_Package (Current_Entity)),
             Make_Defining_Identifier (Name_Find));

         --  record type Declaration

         Args_Type := Make_Full_Type_Declaration
           (Defining_Identifier => N,
            Type_Definition     => Make_Record_Definition (Components),
            Discriminant_Spec   => Discr);

         return Args_Type;
      end Args_Type_Record_In;

      --------------------------
      -- Args_Type_Record_Out --
      --------------------------

      function Args_Type_Record_Out (E : Node_Id) return Node_Id is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         T          : constant Node_Id := Type_Spec (E);
         Param      : constant List_Id := Parameters (E);
         Discr      : constant List_Id := New_List;
         Components : constant List_Id := New_List;
         L          : List_Id := No_List;
         Args_Type  : Node_Id := No_Node;
         Component  : Node_Id;
         Par_Type   : Node_Id;
         N          : Node_Id;
         Par        : Node_Id;
      begin
         --  For each subprogram we generate a record containing the
         --  Out parameters with the aligned type.

         if not FEU.Is_Empty (Param) then

            Par := First_Entity (Param);
            while Present (Par) loop

               if Is_Out (FEN.Parameter_Mode (Par)) then

                  --  If the parameter type is a class-wide type, we
                  --  remove the "'Class" attribute from the type
                  --  name.

                  Par_Type := Type_Spec (Par);
                  if BEN.Kind (Par_Type) = K_Attribute_Reference then
                     Par_Type := Prefix (Par_Type);
                  end if;

                  Par_Type := Make_Type_Designator (Par_Type);

                  if Is_Unbounded_Type (Type_Spec (Par)) then
                     L := New_List;
                     Get_Discriminants (Par_Type, L);
                  end if;

                  Par_Type := Make_Variable_Type
                    (Par_Type, Type_Spec (Par), L);

                  if not Is_Empty (L) then
                     N := First_Node (L);
                     Append_To (Discr, N);
                     L := No_List;
                  end if;

                  Component := Make_Component_Declaration
                    (Defining_Identifier => Make_Defining_Identifier
                     (IDL_Name (Identifier (Declarator (Par)))),
                     Subtype_Indication  => Par_Type);
                  Append_To (Components, Component);
               end if;
               Par := Next_Entity (Par);
            end loop;
         end if;

         --  If the subprogram is a function, we add an additional
         --  member corresponding to the result of the function.

         if Present (T) and then
           FEN.Kind (T) /= K_Void
         then

            --  If the return type is a class-wide type, we remove the
            --  "'Class" attribute from the type name.

            Par_Type := T;
            if BEN.Kind (Par_Type) = K_Attribute_Reference then
               Par_Type := Prefix (Par_Type);
            end if;

            Par_Type := Make_Type_Designator (Par_Type);

            if Is_Unbounded_Type (T) then
               L := New_List;
               Get_Discriminants (T, L, True, False);
            end if;

            Par_Type := Make_Variable_Type (Par_Type, T, L);

            if not Is_Empty (L) then
               N := First_Node (L);
               Append_To (Discr, N);
               L := No_List;
            end if;

            Component := Make_Component_Declaration
              (Defining_Identifier => Make_Defining_Identifier
               (PN (P_Returns)),
               Subtype_Indication  => Par_Type);
            Append_To (Components, Component);
         end if;

         --  The record name

         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_Out");

         N := Make_Selected_Component
           (Defining_Identifier (Aligned_Package (Current_Entity)),
            Make_Defining_Identifier (Name_Find));

         --  Record type Declaration

         Args_Type := Make_Full_Type_Declaration
           (Defining_Identifier => N,
            Type_Definition     => Make_Record_Definition (Components),
            Discriminant_Spec   => Discr);

         return Args_Type;
      end Args_Type_Record_Out;

      --------------------------
      -- Args_Type_Access_Out --
      --------------------------

      function Args_Type_Access_Out (E : Node_Id) return Node_Id
      is
         pragma Assert (FEN.Kind (E) = K_Operation_Declaration);
         Spec       : constant Node_Id := Stub_Node
           (BE_Node (Identifier (E)));
         N : Node_Id;
         M : Node_Id;
      begin
         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_Out");
         N := Make_Defining_Identifier (Name_Find);

         Get_Name_String (BEN.Name (Defining_Identifier (Spec)));
         Add_Str_To_Name_Buffer ("_Args_Type_Access_Out");

         M := Make_Selected_Component
            (Defining_Identifier (Aligned_Package (Current_Entity)),
             Make_Defining_Identifier (Name_Find));

         M := Make_Full_Type_Declaration
           (Defining_Identifier => M,
            Type_Definition     => Make_Access_Type_Definition (N));

         return M;
      end Args_Type_Access_Out;

      -----------
      -- Visit --
      -----------

      procedure Visit (E : Node_Id) is
      begin
         case FEN.Kind (E) is

            when K_Module =>
               Visit_Module (E);

            when K_Operation_Declaration =>
               Visit_Operation_Declaration (E);

            when K_Interface_Declaration =>
               Visit_Interface_Declaration (E);

            when K_Structure_Type =>
               Visit_Structure_Type (E);

            when K_Union_Type =>
               Visit_Union_Type (E);

            when K_Type_Declaration =>
               Visit_Type_Declaration (E);

            when K_Specification =>
               Visit_Specification (E);

            when others =>
               null;
         end case;
      end Visit;

      ----------------------------
      -- Visit_Type_Declaration --
      ----------------------------

      procedure Visit_Type_Declaration (E : Node_Id) is
         D                : Node_Id;
         Id               : Node_Id;
         T                : Node_Id;
         N                : Node_Id;
         Type_Spec_Node   : Node_Id;
         Is_Subtype       : Boolean := FEN.Marked_As_Subtype (E);
      begin
         Set_Aligned_Spec;
         Type_Spec_Node := Type_Spec (E);

         --  * The fixed type shall be mapped to an equivalent Ada
         --  decimal type.

         --  * For each declarator, a type definition shall be
         --  generated.

         if FEN.Kind (Type_Spec_Node) = K_Fixed_Point_Type then
            declare
               Fixed_Type_Node : Node_Id;
               Fixed_Name      : constant Name_Id
                 := Map_Fixed_Type_Name (Type_Spec_Node);
            begin
               --  XXX it is certainly false.
               --  TODO: make a package instantiation at the marshalling time

               T := Make_Selected_Component
                 (Defining_Identifier (Aligned_Package (Current_Entity)),
                  Make_Defining_Identifier (Fixed_Name));

               Fixed_Type_Node := Make_Full_Type_Declaration
                 (Defining_Identifier => T,
                  Type_Definition     => Make_Decimal_Type_Definition
                                           (Type_Spec_Node),
                  Is_Subtype          => Is_Subtype);

               Append_To (Visible_Part (Current_Package), Fixed_Type_Node);
            end;

         elsif FEN.Kind (Type_Spec_Node) = K_String_Type or else
           FEN.Kind (Type_Spec_Node) = K_Wide_String_Type
         then
            declare
               Str_Package_Inst : Node_Id;
               Pkg_Name         : Name_Id;
               Pkg_Node         : Node_Id;
               String_Pkg       : Node_Id;
            begin
               --  We create an instantiation of the generic package
               --  PolyORB.Aligned_Types.Bounded_Strings (or
               --  PolyORB.Aligned_Types.Bounded_Wide_Strings). Then,
               --  the string type is derived from the
               --  'Bounded_String' type (or the 'Bounded_Wide_String'
               --  type of the instantiated package.

               Pkg_Name := Map_String_Pkg_Name (Type_Spec_Node);

               if FEN.Kind (Type_Spec_Node) = K_Wide_String_Type then
                  String_Pkg :=
                    RU (RU_PolyORB_Aligned_Types_Bounded_Wide_Strings,
                        False);
                  T := Make_Defining_Identifier (TN (T_Bounded_Wide_String));
               else
                  String_Pkg :=
                    RU (RU_PolyORB_Aligned_Types_Bounded_Strings,
                        False);
                  T := Make_Defining_Identifier (TN (T_Bounded_String));
               end if;

               --  Building the string package node

               Pkg_Node := Make_Selected_Component
                 (Defining_Identifier (Aligned_Package (Current_Entity)),
                  Make_Defining_Identifier (Pkg_Name));

               Str_Package_Inst := Make_Package_Instantiation
                 (Defining_Identifier => Pkg_Node,
                  Generic_Package     => String_Pkg,
                  Parameter_List      => New_List
                  (Make_Literal (FEU.Expr_Value (Max_Size (Type_Spec_Node)))));

               Append_To (Visible_Part (Current_Package), Str_Package_Inst);

               T := Make_Selected_Component (Pkg_Node, T);
            end;

         elsif FEN.Kind (Type_Spec_Node) /= K_Sequence_Type then
            --  General case

            T := Make_Type_Designator (Type_Spec_Node);
         end if;

         Is_Subtype := Is_Subtype or Is_Object_Type (Type_Spec (E));

         D := First_Entity (Declarators (E));

         while Present (D) loop
            Id := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Map_Defining_Identifier (D));

            if Kind (D) = K_Complex_Declarator then
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Id,
                  Type_Definition     =>
                    Make_Array_Type_Definition
                  (Map_Range_Constraints
                   (FEN.Array_Sizes (D))
                   , T));
            elsif FEN.Kind (Type_Spec_Node) = K_Sequence_Type then
               declare
                  M    : Node_Id;
                  K    : Node_Id;
                  Rang : Node_Id;
                  L    : constant List_Id := New_List;
                  Disc : constant List_Id := New_List;
               begin
                  --  Declaration of array of element type

                  M := Make_Type_Designator (Type_Spec (Type_Spec_Node));
                  K := RE (RE_Unsigned_Long_10);
                  M := Make_Array_Type_Definition (No_List, M, K);

                  K := Map_Defining_Identifier (D);
                  Set_Str_To_Name_Buffer
                    (Get_Name_String (BEN.Name (K)) & "_Content");

                  K := Make_Defining_Identifier (Name_Find);
                  M := Make_Full_Type_Declaration (K, M);
                  Append_To (Visible_Part (Current_Package), M);

                  --  Declration of the sequence type

                  if Present (Max_Size (Type_Spec_Node)) then
                     K := Make_Literal
                       (FEU.Expr_Value (Max_Size (Type_Spec_Node)));
                  else
                     M := Map_Defining_Identifier (D);
                     Set_Str_To_Name_Buffer
                       (Get_Name_String (BEN.Name (M)) & "_Length");
                     K := Make_Defining_Identifier (Name_Find);
                  end if;

                  Rang := Make_Range_Constraint (Make_Literal (Int1_Val), K);

                  M := Map_Defining_Identifier (D);
                  Set_Str_To_Name_Buffer
                    (Get_Name_String (BEN.Name (M)) & "_Content");
                  M := Make_Defining_Identifier (Name_Find);
                  K := Make_String_Type_Definition (M, Rang);

                  M := Make_Component_Declaration
                    (Make_Defining_Identifier (PN (P_Content)), K);
                  Append_To (L, M);

                  --  Discriminant

                  if not Present (Max_Size (Type_Spec_Node)) then
                     K := Map_Defining_Identifier (D);
                     Set_Str_To_Name_Buffer
                       (Get_Name_String (BEN.Name (K)) & "_Length");
                     M := Make_Defining_Identifier (Name_Find);

                     M := Make_Component_Declaration
                       (M, RE (RE_Unsigned_Long_10));
                     Append_To (Disc, M);
                  end if;

                  --  Type declaration

                  N := Make_Full_Type_Declaration
                    (Defining_Identifier => Id,
                     Type_Definition     => Make_Record_Definition (L),
                     Discriminant_Spec   => Disc);
               end;
            else
               N := Make_Full_Type_Declaration
                 (Defining_Identifier => Id,
                  Type_Definition     => Make_Derived_Type_Definition
                  (Subtype_Indication    => T,
                   Record_Extension_Part => No_Node,
                   Is_Subtype => Is_Subtype,
                   Opt_Range => Optional_Range (E)),
                  Is_Subtype => Is_Subtype);
            end if;

            Append_To (Visible_Part (Current_Package), N);

            D := Next_Entity (D);
         end loop;
      end Visit_Type_Declaration;

      ---------------------------------
      -- Visit_Interface_Declaration --
      ---------------------------------

      procedure Visit_Interface_Declaration (E : Node_Id) is
         N : Node_Id;
      begin
         --  If local interface, nothing to do.

         if FEN.Is_Local_Interface (E) then
            return;
         end if;

         N := BEN.Parent (Type_Def_Node (BE_Node (Identifier (E))));
         Push_Entity (BEN.IDL_Unit (Package_Declaration (N)));
         Set_Aligned_Spec;

         N := First_Entity (Interface_Body (E));

         while Present (N) loop
            Visit (N);
            N := Next_Entity (N);
         end loop;

         Pop_Entity;
      end Visit_Interface_Declaration;

      --------------------------
      -- Visit_Structure_Type --
      --------------------------

      procedure Visit_Structure_Type (E : Node_Id) is
         Components : constant List_Id := New_List;
         Discr      : constant List_Id := New_List;
         L          :          List_Id := New_List;
         N          : Node_Id;
         M          : Node_Id;
         C          : Node_Id;
         Member     : Node_Id;
         Unbounded  : Boolean;
      begin
         Set_Aligned_Spec;
         Member := First_Entity (Members (E));

         while Present (Member) loop
            N := Map_Defining_Identifier
              (Identifier (First_Entity (Declarators (Member))));

            M := Make_Type_Designator (Type_Spec (Member),
                                       First_Entity (Declarators (Member)));

            Unbounded := Is_Unbounded_Type (Type_Spec (Member));

            if Unbounded then
               Get_Discriminants (Member, L, False, True);
               M := Make_Variable_Type (M, Type_Spec (Member), L);
               C := First_Node (L);
               Append_To (Discr, C);
               L := New_List;
            end if;

            N := Make_Component_Declaration
              (Defining_Identifier => N,
               Subtype_Indication  => M);
            Append_To (Components, N);

            Member := Next_Entity (Member);
         end loop;

         N := Make_Selected_Component
           (Defining_Identifier (Aligned_Package (Current_Entity)),
            Make_Defining_Identifier (FEN.IDL_Name (FEN.Identifier (E))));

         N := Make_Full_Type_Declaration
           (Defining_Identifier   => N,
            Type_Definition       => Make_Record_Definition (Components),
            Discriminant_Spec     => Discr);
         Append_To (Visible_Part (Current_Package), N);
      end Visit_Structure_Type;

      ----------------------
      -- Visit_Union_Type --
      ----------------------

      procedure Visit_Union_Type (E : Node_Id) is
         N                   : Node_Id;
         S                   : Node_Id := Switch_Type_Spec (E);
         Orig_Type           : constant Node_Id :=
           FEU.Get_Original_Type_Specifier (S);
         L                   : List_Id;
         Discr               : List_Id;
         Components          : List_Id;
         Choices             : List_Id;
         Variants            : List_Id;
         Literal_Parent      : Node_Id := No_Node;
         T                   : Node_Id;
         Switch_Case         : Node_Id;
         M                   : Node_Id;
         Variant             : Node_Id;
         Label               : Node_Id;
         Choice              : Node_Id;
      begin
         Set_Aligned_Spec;
         T := Make_Type_Designator (S);

         --  If the discriminator is an enumeration type, we must put
         --  the full names of the literal.

         if FEN.Kind (Orig_Type) = K_Enumeration_Type then
            Literal_Parent := Map_Expanded_Name
              (Scope_Entity
               (Identifier
                (Orig_Type)));
         else
            S := No_Node;
         end if;

         L := New_List;
         Discr := New_List;
         Components := New_List;

         Variants := New_List;
         Switch_Case := First_Entity (Switch_Type_Body (E));

         while Present (Switch_Case) loop
            Variant := New_Node (K_Variant);
            Choices := New_List;
            Set_Discrete_Choices (Variant, Choices);

            --  Make the switchs

            --  Expansion guarantees that the "default:" case is
            --  isolated in a standalone alternative.

            Label := First_Entity (Labels (Switch_Case));

            while Present (Label) loop
               Choice := Make_Literal_With_Parent
                 (Value  => FEU.Expr_Value (Label),
                  Parent => Literal_Parent);
               if S /= No_Node then
                  Choice := Cast_Variable_To_PolyORB_Aligned_Type (Choice, S);
               end if;

               Append_To (Choices, Choice);
               Label := Next_Entity (Label);
            end loop;

            N := Map_Defining_Identifier
              (Identifier (Declarator (Element (Switch_Case))));

            M := Make_Type_Designator
              (Type_Spec (Element (Switch_Case)),
               Declarator (Element (Switch_Case)));

            if Is_Unbounded_Type (Type_Spec (Element (Switch_Case))) then
               Get_Discriminants (Element (Switch_Case), L);
               M := Make_Variable_Type
                 (M, Type_Spec (Element (Switch_Case)), L);
               Append_To (Discr, First_Node (L));
               L := New_List;
            end if;

            Set_Component (Variant, Make_Component_Declaration (N, M));

            Append_To (Variants, Variant);
            Switch_Case := Next_Entity (Switch_Case);
         end loop;

         Append_To (Discr,
           Make_Component_Declaration
             (Make_Defining_Identifier (CN (C_Switch)), T,
              Make_Attribute_Reference (T, A_First)));

         Append_To (Components,
           Make_Variant_Part
             (Make_Defining_Identifier (CN (C_Switch)),
              Variants));

         --  Type declaration

         N := Make_Full_Type_Declaration
           (Make_Defining_Identifier (FEN.IDL_Name (FEN.Identifier (E))),
            Make_Record_Type_Definition
            (Make_Record_Definition (Components)),
            Discr);

         Append_To (Visible_Part (Current_Package), N);
      end Visit_Union_Type;

      ------------------
      -- Visit_Module --
      ------------------

      procedure Visit_Module (E : Node_Id) is
         D : Node_Id;
      begin
         if not Map_Particular_CORBA_Parts (E, PK_Aligned_Spec) then
            Push_Entity (Stub_Node (BE_Node (Identifier (E))));
            D := First_Entity (Definitions (E));

            while Present (D) loop
               Visit (D);
               D := Next_Entity (D);
            end loop;

            Pop_Entity;
         end if;
      end  Visit_Module;

      ---------------------------------
      -- Visit_Operation_Declaration --
      ---------------------------------

      procedure Visit_Operation_Declaration (E : Node_Id) is
         N     : Node_Id;
      begin
         Set_Aligned_Spec;
         Set_Str_To_Name_Buffer ("Operation : ");
         Get_Name_String_And_Append (IDL_Name (Identifier (E)));
         N := Make_Ada_Comment (Name_Find);
         Append_To (Visible_Part (Current_Package), N);

         --  Generating the 'Operation_Name'_Args_Type_In/Out
         --  declarations.

         N := Args_Type_Record_In (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Args_In);

         N := Args_Type_Record_Out (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Args_Out);

         N := Args_Type_Access_Out (E);
         Append_To (Visible_Part (Current_Package), N);
         Bind_FE_To_BE (Identifier (E), N, B_Access_Args_Out);
      end Visit_Operation_Declaration;

      -------------------------
      -- Visit_Specification --
      -------------------------

      procedure Visit_Specification (E : Node_Id) is
         Definition : Node_Id;
      begin
         Push_Entity (Stub_Node (BE_Node (Identifier (E))));
         Definition := First_Entity (Definitions (E));

         while Present (Definition) loop
            Visit (Definition);
            Definition := Next_Entity (Definition);
         end loop;

         Pop_Entity;
      end Visit_Specification;

      ------------------------
      -- Make_Variable_Type --
      ------------------------

      function Make_Variable_Type
        (N         : Node_Id;
         Type_Spec : Node_Id;
         Desc      : List_Id)
        return Node_Id
      is
         Rewinded_Type : Node_Id;
         M             : Node_Id;
      begin
         if Is_Empty (Desc) then
            return N;
         end if;

         Rewinded_Type := FEU.Get_Original_Type_Specifier (Type_Spec);
         Set_Aligned_Spec;

         case FEN.Kind (Rewinded_Type) is
            when K_String
              | K_Wide_String =>
               M := Make_Subprogram_Call
                 (N,
                  New_List (Defining_Identifier (First_Node (Desc))));
               return M;

            when K_Sequence_Type =>
               if not Present (Max_Size (Rewinded_Type)) then
                  M := Make_Subprogram_Call
                    (N,
                     New_List (Defining_Identifier (First_Node (Desc))));
                  return M;
               else
                  return N;
               end if;

            when others =>
               declare
                  K : Node_Id;
                  L : List_Id;
               begin
                  --  Make Union_Type (Switch, Discriminant1, ...,
                  --  DiscriminantsN);

                  L := New_List;
                  K := First_Node (Desc);

                  while Present (K) loop
                     M := Make_Identifier
                       (Fully_Qualified_Name (Defining_Identifier (K)));
                     Append_To (L, M);
                     K := Next_Node (K);
                  end loop;

                  M := Make_Subprogram_Call (N, L);
                  return M;
               end;
         end case;
      end Make_Variable_Type;

      -----------------------
      -- Is_Unbounded_Type --
      -----------------------

      function Is_Unbounded_Type (N : Node_Id) return Boolean is
         Rewinded_Type : Node_Id;
      begin
         Rewinded_Type := FEU.Get_Original_Type_Specifier (N);
         case FEN.Kind (Rewinded_Type) is

            when K_Long
              | K_Unsigned_Short
              | K_Unsigned_Long
              | K_Long_Long
              | K_Unsigned_Long_Long
              | K_Short
              | K_Float
              | K_Double
              | K_Long_Double
              | K_Boolean
              | K_Char
              | K_Wide_Char
              | K_Octet
              | K_String_Type
              | K_Wide_String_Type =>
               return False;

            when K_String
              | K_Wide_String
              | K_Sequence_Type
              | K_Union_Type =>
               return True;

            when K_Structure_Type =>
               declare
                  Ret    : Boolean := False;
                  Member : Node_Id;
               begin
                  --  We test if there is an unbounded member

                  Member := First_Entity (Members (Rewinded_Type));

                  while Present (Member) loop
                     Ret := Is_Unbounded_Type (Type_Spec (Member));

                     exit when Ret;

                     Member := Next_Entity (Member);
                  end loop;

                  return Ret;
               end;

            when others =>
               return False;
         end case;
      end Is_Unbounded_Type;

      -----------------------
      -- Get_Discriminants --
      -----------------------

      procedure Get_Discriminants
        (N      : Node_Id;
         L      : List_Id;
         Ret    : Boolean := False;
         Struct : Boolean := False)
      is
         Rewinded_Type : Node_Id;
         M             : Node_Id;
      begin
         --  If we are processing the return value we have directly
         --  the type.

         if Ret then
            Rewinded_Type := FEU.Get_Original_Type_Specifier (N);
         else
            Rewinded_Type := FEU.Get_Original_Type_Specifier (Type_Spec (N));
         end if;

         case FEN.Kind (Rewinded_Type) is

            when K_Union_Type =>
               declare
                  Member : Node_Id;
               begin
                  Member := First_Entity (Switch_Type_Body (Rewinded_Type));

                  while Present (Member) loop
                     if Is_Unbounded_Type (Type_Spec (Element (Member))) then
                        Get_Discriminants (Element (Member), L);
                     end if;

                     Member := Next_Entity (Member);
                  end loop;

                  M := Make_Type_Designator (Switch_Type_Spec (Rewinded_Type));

                  M := Make_Component_Declaration
                    (Make_Defining_Identifier (CN (C_Switch)), M,
                     Make_Attribute_Reference (M, A_First));
                  Append_To (L, M);
               end;

            when K_Structure_Type =>
               declare
                  Member : Node_Id;
               begin
                  Member := First_Entity (Members (Rewinded_Type));

                  while Present (Member) loop
                     if Is_Unbounded_Type (Type_Spec (Member)) then
                        Get_Discriminants (Member, L, False, True);
                     end if;

                     Member := Next_Entity (Member);
                  end loop;
               end;

            when K_String
              | K_Wide_String =>

               if Struct then
                  Get_Name_String
                    (IDL_Name (Identifier (First_Entity (Declarators (N)))));
               elsif Ret then
                  Set_Str_To_Name_Buffer ("Returns");
               else
                  Get_Name_String
                    (IDL_Name (Identifier (Declarator (N))));
               end if;

               Add_Str_To_Name_Buffer ("_Size");
               M := Make_Defining_Identifier (Name_Find);
               M := Make_Component_Declaration
                 (M, RE (RE_Natural),
                  Make_Attribute_Reference (RE (RE_Natural), A_First));

               Append_To (L, M);

            when K_Sequence_Type =>

               if Present (Max_Size (Rewinded_Type)) then
                  return;
               end if;

               if Struct then
                  Get_Name_String
                    (IDL_Name (Identifier (First_Entity (Declarators (N)))));
               elsif Ret then
                  Set_Str_To_Name_Buffer ("Returns");
               else
                  Get_Name_String
                    (IDL_Name (Identifier (Declarator (N))));
               end if;
               Add_Str_To_Name_Buffer ("_Size");
               M := Make_Defining_Identifier (Name_Find);

               Append_To (L,
                 Make_Component_Declaration (M, RE (RE_Unsigned_Long_10)));

            when others =>
               null;
         end case;
      end Get_Discriminants;

   end Package_Spec;
end Backend.BE_CORBA_Ada.Aligned;
