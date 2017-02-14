------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--          B A C K E N D . B E _ C O R B A _ A D A . C O M M O N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2013, Free Software Foundation, Inc.          --
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
with Values;
with Frontend.Nodes;  use Frontend.Nodes;
with Frontend.Nutils;

with Backend.BE_CORBA_Ada.Nutils;      use Backend.BE_CORBA_Ada.Nutils;
with Backend.BE_CORBA_Ada.Nodes;       use Backend.BE_CORBA_Ada.Nodes;
with Backend.BE_CORBA_Ada.IDL_To_Ada;  use Backend.BE_CORBA_Ada.IDL_To_Ada;
with Backend.BE_CORBA_Ada.Runtime;     use Backend.BE_CORBA_Ada.Runtime;

package body Backend.BE_CORBA_Ada.Common is

   package FEN renames Frontend.Nodes;
   package FEU renames Frontend.Nutils;

   -------------------------------------
   -- Cast_Variable_From_PolyORB_Type --
   -------------------------------------

   function Cast_Variable_From_PolyORB_Type
     (Var_Name : Name_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N                : Node_Id;
      Orig_Type        : Node_Id;
      Direct_Type_Node : Node_Id;
   begin
      N := Make_Identifier (Var_Name);

      Orig_Type := FEU.Get_Original_Type_Specifier (Var_Type);

      if FEN.Kind (Var_Type) = K_Simple_Declarator
        or else FEN.Kind (Var_Type) = K_Complex_Declarator
      then
         Direct_Type_Node := Type_Spec (Declaration (Var_Type));
      else
         Direct_Type_Node := Var_Type;
      end if;

      case FEN.Kind (Orig_Type) is

         when K_String =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String_1),
                  New_List (N));

               if FEN.Kind (Direct_Type_Node) /= K_String then
                  declare
                     Ada_Type : constant Node_Id :=
                       Map_Expanded_Name (Direct_Type_Node);

                     TCS : constant Node_Id := Make_Selected_Component
                       (Copy_Node (Prefix (Ada_Type)),
                        Make_Identifier (SN (S_To_CORBA_String)));
                     --  To_CORBA_String primitive inherited from the
                     --  CORBA.String type.
                  begin
                     N := Make_Subprogram_Call (TCS, New_List (N));

                     --  We use qualified expression to avoid conflicts with
                     --  types derived from String in the spec of package CORBA
                     --  (RepositoryId, ScopedName...)

                     N := Make_Qualified_Expression (Ada_Type, N);
                  end;
               else
                  N := Make_Subprogram_Call
                    (RE (RE_To_CORBA_String),
                     New_List (N));
                  N := Make_Qualified_Expression (RE (RE_String_0), N);
               end if;
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (SN (S_To_Bounded_String)));

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String_1),
                  New_List (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  New_List (N));

               N := Make_Subprogram_Call
                 (Map_Expanded_Name (Direct_Type_Node),
                  New_List (N));
            end;

         when K_Wide_String =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String_1),
                  New_List (N));

               if FEN.Kind (Direct_Type_Node) /= K_Wide_String then
                  declare
                     Ada_Type : constant Node_Id :=
                       Map_Expanded_Name (Direct_Type_Node);

                     TCWS : constant Node_Id := Make_Selected_Component
                       (Copy_Node (Prefix (Ada_Type)),
                        Make_Identifier (SN (S_To_CORBA_Wide_String)));
                     --  To_CORBA_Wide_String primitive inherited from the
                     --  CORBA.Wide_String type.
                  begin
                     N := Make_Subprogram_Call (TCWS, New_List (N));
                     N := Make_Qualified_Expression (Ada_Type, N);
                  end;
               else
                  N := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Wide_String),
                     New_List (N));
                  N := Make_Qualified_Expression (RE (RE_Wide_String), N);
               end if;
            end;

         when K_Wide_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Convert_Subp := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (SN (S_To_Bounded_Wide_String)));

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String_1),
                  New_List (N));

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  New_List (N));

               N := Make_Subprogram_Call
                 (Map_Expanded_Name (Direct_Type_Node),
                  New_List (N));
            end;

         when K_Long
           | K_Long_Long
           | K_Unsigned_Long
           | K_Unsigned_Long_Long
           | K_Float
           | K_Double
           | K_Long_Double
           | K_Char
           | K_Wide_Char
           | K_Octet
           | K_Sequence_Type
           | K_Short
           | K_Unsigned_Short
           | K_Boolean
           | K_Fixed_Point_Type
           | K_Any =>
            declare
               CORBA_Type : constant Node_Id := Map_Expanded_Name
                 (Direct_Type_Node);
            begin
               N := Make_Subprogram_Call
                 (CORBA_Type,
                  New_List (N));
            end;

            --  For Objects and interfaces, there is no need to cast
            --  to the original type because the type definition is
            --  done by means of 'subtype' and not 'type ... is new
            --  ...'

         when K_Object =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_Ref),
                  New_List (N));
            end;

         when K_Interface_Declaration |  K_Forward_Interface_Declaration =>
            --  Check whether we are dealing with a TypeCode

            if Get_Predefined_CORBA_Entity (Orig_Type) = RE_Object then
               N := Make_Subprogram_Call
                 (RE (RE_To_CORBA_Object),
                  New_List
                  (N));
            else
               declare
                  To_Ref_Node : constant Node_Id := Get_To_Ref_Node
                    (Direct_Type_Node);
               begin
                  N := Make_Subprogram_Call
                    (RE (RE_To_CORBA_Ref),
                     New_List (N));
                  N := Make_Subprogram_Call
                    (To_Ref_Node,
                     New_List (N));
               end;
            end if;

         when K_Enumeration_Type =>
            declare
               CORBA_Type : constant Node_Id := Map_Expanded_Name
                 (Direct_Type_Node);
               M : Node_Id;
            begin
               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Val attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Attribute_Reference (CORBA_Type, A_Val);
               N := Make_Subprogram_Call (M, New_List (N));
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_From_PolyORB_Type;

   -----------------------------------
   -- Cast_Variable_To_PolyORB_Type --
   -----------------------------------

   function Cast_Variable_To_PolyORB_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N         : Node_Id;
      Orig_Type : Node_Id;
   begin
      N := Var_Node;

      Orig_Type := FEU.Get_Original_Type_Specifier (Var_Type);

      case FEN.Kind (Orig_Type) is

         when K_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_1), N);
            end;

         when K_Long_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_Long_1), N);
            end;

         when K_Unsigned_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_1), N);
            end;

         when K_Unsigned_Long_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_Long_1), N);
            end;

         when K_Short =>
            begin
               N := Make_Type_Conversion (RE (RE_Short_1), N);
            end;

         when K_Unsigned_Short =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Short_1), N);
            end;

         when K_Float =>
            begin
               N := Make_Type_Conversion (RE (RE_Float_1), N);
            end;

         when K_Double =>
            begin
               N := Make_Type_Conversion (RE (RE_Double_1), N);
            end;

         when K_Long_Double =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_Double_1), N);
            end;

         when K_Char =>
            begin
               N := Make_Type_Conversion (RE (RE_Char_1), N);
            end;

         when K_Wide_Char =>
            begin
               N := Make_Type_Conversion (RE (RE_Wchar_1), N);
            end;

         when K_Octet =>
            begin
               N := Make_Type_Conversion (RE (RE_Octet_1), N);
            end;

         when K_Boolean =>
            begin
               N := Make_Type_Conversion (RE (RE_Boolean_1), N);
            end;

         when K_Fixed_Point_Type =>
            declare
               FP_Type_Node     : Node_Id;
            begin

               --  Getting the fixed point type

               FP_Type_Node := Expand_Designator
                 (Type_Def_Node (BE_Node (Orig_Type)));

               N := Make_Type_Conversion (FP_Type_Node, N);
            end;

         when K_Object =>
            begin
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Ref), New_List (N));
            end;

         when K_Interface_Declaration | K_Forward_Interface_Declaration =>
            --  Check whether we are dealing with a TypeCode

            if Get_Predefined_CORBA_Entity (Orig_Type) = RE_Object then
               N := Make_Subprogram_Call
                 (RE (RE_Object_Of_1),
                  New_List
                  (Make_Subprogram_Call
                   (RE (RE_To_PolyORB_Object),
                    New_List (N))));
            else
               N := Make_Type_Conversion (RE (RE_Ref_2), N);
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Ref), New_List (N));
            end if;

         when K_Enumeration_Type =>
            declare
               Ada_Enum_Type : constant Node_Id := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Orig_Type))));
               M : Node_Id;
            begin
               if FEN.Kind (Var_Type) = K_Scoped_Name
                 and then FEN.Kind (Reference (Var_Type))
                 /= K_Enumeration_Type
               then
                  N := Make_Type_Conversion (Ada_Enum_Type,
                     N);
               end if;

               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Pos' attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Attribute_Reference (Ada_Enum_Type, A_Pos);
               M := Make_Subprogram_Call (M, New_List (N));
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_1), M);
            end;

         when K_String =>
            begin
               if FEN.Kind (Var_Type) /= K_String then
                  N := Make_Type_Conversion (RE (RE_String_0), N);
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String),
                  New_List (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  New_List (N));
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (TN (T_Bounded_String)));

               Str_Convert_Subp := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (SN (S_To_String)));

               N := Make_Type_Conversion (Str_Type, N);

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  New_List (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  New_List (N));
            end;

         when K_Wide_String =>
            begin
               if FEN.Kind (Var_Type) /= K_Wide_String then
                  N := Make_Type_Conversion (RE (RE_Wide_String), N);
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_Wide_String),
                  New_List (N));
               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Wide_String),
                  New_List (N));
            end;

         when K_Wide_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (TN (T_Bounded_Wide_String)));

               Str_Convert_Subp := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (SN (S_To_Wide_String)));

               N := Make_Type_Conversion (Str_Type, N);

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  New_List (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_Wide_String),
                  New_List (N));
            end;

         when K_Sequence_Type =>
            declare
               Seq_Package_Node : Node_Id;
               Seq_Type         : Node_Id;
            begin

               --  Getting the instantiated package node

               Seq_Package_Node := Expand_Designator
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Sequence type

               Seq_Type := Make_Selected_Component
                 (Seq_Package_Node,
                  Make_Identifier (TN (T_Sequence)));

               N := Make_Type_Conversion (Seq_Type, N);
            end;

         when K_Any =>
            begin
               N := Make_Type_Conversion (RE (RE_Any_1), N);
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_To_PolyORB_Type;

   -----------
   -- Is_In --
   -----------

   function Is_In (Par_Mode : Mode_Id) return Boolean is
   begin
      return Par_Mode = Mode_In or else Par_Mode = Mode_Inout;
   end Is_In;

   ------------
   -- Is_Out --
   ------------

   function Is_Out (Par_Mode : Mode_Id) return Boolean is
   begin
      return Par_Mode = Mode_Out or else Par_Mode = Mode_Inout;
   end Is_Out;

   ----------------------------
   -- Contains_In_Parameters --
   ----------------------------

   function Contains_In_Parameters (E : Node_Id) return Boolean is
      pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

      Parameter : Node_Id;
      Result    : Boolean := False;
   begin
      Parameter := First_Entity (Parameters (E));

      while Present (Parameter) loop
         if Is_In (FEN.Parameter_Mode (Parameter)) then
            Result := True;
            exit;
         end if;

         Parameter := Next_Entity (Parameter);
      end loop;

      return Result;
   end Contains_In_Parameters;

   -----------------------------
   -- Contains_Out_Parameters --
   -----------------------------

   function Contains_Out_Parameters (E : Node_Id) return Boolean is
      pragma Assert (FEN.Kind (E) = K_Operation_Declaration);

      Parameter : Node_Id;
      Result    : Boolean := False;
   begin
      Parameter := First_Entity (Parameters (E));

      while Present (Parameter) loop
         if Is_Out (FEN.Parameter_Mode (Parameter)) then
            Result := True;
            exit;
         end if;

         Parameter := Next_Entity (Parameter);
      end loop;

      return Result;
   end Contains_Out_Parameters;

   --------------------------
   -- Make_type_Designator --
   --------------------------

   function Make_Type_Designator
     (N          : Node_Id;
      Declarator : Node_Id := No_Node)
     return Node_Id
   is
      Rewinded_Type : Node_Id;
      M             : Node_Id;
   begin
      Set_Aligned_Spec;
      Rewinded_Type := FEU.Get_Original_Type_Specifier (N);

      if Present (Declarator) and then
        FEN.Kind (Declarator) = K_Complex_Declarator
      then
         declare
            Designator : Node_Id;
            Decl_Name  : Name_Id;
            Type_Node  : Node_Id;
         begin
            Decl_Name := To_Ada_Name
              (IDL_Name (FEN.Identifier (Declarator)));
            Designator := Make_Type_Designator (N);

            Get_Name_String (Decl_Name);
            Add_Str_To_Name_Buffer ("_Array");
            Decl_Name := Name_Find;

            Type_Node := Make_Full_Type_Declaration
              (Defining_Identifier => Make_Defining_Identifier (Decl_Name),
               Type_Definition     => Make_Array_Type_Definition
               (Map_Range_Constraints
                (FEN.Array_Sizes (Declarator)), Designator));

            --  We make a link between the identifier and the type
            --  declaration.  This link is useful for the generation
            --  of the From_Any and To_Any functions and the TC_XXX
            --  constant necessary for user defined types.

            Append_To (Visible_Part (Current_Package), Type_Node);

            Designator := Make_Selected_Component
              (Defining_Identifier (Stubs_Package (Current_Entity)),
               Defining_Identifier (Type_Node));

            return Designator;
         end;
      end if;

      case FEN.Kind (Rewinded_Type) is

         when K_String =>
            return RE (RE_String_10);

         when K_Sequence_Type =>
            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (IDL_Name (Identifier (N))));
            return M;

         when K_Long =>
            return RE (RE_Long_10);

         when K_Short =>
            return RE (RE_Short_10);

         when K_Boolean =>
            return RE (RE_Boolean_10);

         when K_Octet =>
            return RE (RE_Octet_10);

         when K_Char =>
            return RE (RE_Char_10);

         when K_Wide_Char =>
            return RE (RE_Wchar_10);

         when K_Unsigned_Short =>
            return RE (RE_Unsigned_Short_10);

         when K_Unsigned_Long
           | K_Enumeration_Type =>
            return RE (RE_Unsigned_Long_10);

         when K_Long_Long =>
            return RE (RE_Long_Long_10);

         when K_Unsigned_Long_Long =>
            return RE (RE_Unsigned_Long_Long_10);

         when K_Long_Double =>
            return RE (RE_Long_Double_10);

         when K_Float =>
            return RE (RE_Float_10);

         when K_Double =>
            return RE (RE_Double_10);

         when K_Complex_Declarator =>
            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (IDL_Name (Identifier (N))));
            return M;

         when K_String_Type
           | K_Wide_String_Type
           | K_Structure_Type
           | K_Union_Type
           | K_Fixed_Point_Type =>

            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (IDL_Name (Identifier (N))));
            return M;

         when K_Object =>
            --  XXX is it right ?

            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (FEN.Image (Base_Type (Rewinded_Type))));
            return M;

         when K_Interface_Declaration =>
            --  XXX is it right ?

            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (IDL_Name (Identifier (Rewinded_Type))));
            return M;

         when others =>
            --  If any problem print the node kind here
            raise Program_Error;
      end case;
   end Make_Type_Designator;

   -------------------------------------------
   -- Cast_Variable_To_PolyORB_Aligned_Type --
   -------------------------------------------

   function Cast_Variable_To_PolyORB_Aligned_Type
     (Var_Node : Node_Id; Var_Type : Node_Id)
     return Node_Id
   is
      N         : Node_Id;
      Orig_Type : Node_Id;
   begin
      N := Var_Node;

      Orig_Type := FEU.Get_Original_Type_Specifier (Var_Type);

      case FEN.Kind (Orig_Type) is

         when K_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_10), N);
            end;

         when K_Long_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_Long_10), N);
            end;

         when K_Unsigned_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_10), N);
            end;

         when K_Unsigned_Long_Long =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_Long_10), N);
            end;

         when K_Short =>
            begin
               N := Make_Type_Conversion (RE (RE_Short_10), N);
            end;

         when K_Unsigned_Short =>
            begin
               N := Make_Type_Conversion (RE (RE_Unsigned_Short_10), N);
            end;

         when K_Float =>
            begin
               N := Make_Type_Conversion (RE (RE_Float_10), N);
            end;

         when K_Double =>
            begin
               N := Make_Type_Conversion (RE (RE_Double_10), N);
            end;

         when K_Long_Double =>
            begin
               N := Make_Type_Conversion (RE (RE_Long_Double_10), N);
            end;

         when K_Char =>
            begin
               N := Make_Type_Conversion (RE (RE_Char_10), N);
            end;

         when K_Octet =>
            begin
               N := Make_Type_Conversion (RE (RE_Octet_10), N);
            end;

         when K_Boolean =>
            begin
               N := Make_Type_Conversion (RE (RE_Boolean_10), N);
            end;

         when K_Fixed_Point_Type =>
            declare
               FP_Type_Node     : Node_Id;
            begin
               --  Getting the fixed point type

               FP_Type_Node := Expand_Designator
                 (Type_Def_Node (BE_Node (Orig_Type)));

               N := Make_Type_Conversion (FP_Type_Node, N);
            end;

         when K_Enumeration_Type =>
            declare
               Ada_Enum_Type : constant Node_Id := Expand_Designator
                 (Type_Def_Node
                  (BE_Node
                   (Identifier
                    (Orig_Type))));
               M : Node_Id;
            begin
               if FEN.Kind (Var_Type) = K_Scoped_Name
                 and then FEN.Kind (Reference (Var_Type))
                 /= K_Enumeration_Type
               then
                  N := Make_Type_Conversion (Ada_Enum_Type, N);
               end if;

               --  Even if the type is not directly an enumeration and
               --  is defined basing on an enumeration, we still have
               --  access to the 'Pos' attribute. So there is no need
               --  to cast the variable to the original enumeration
               --  type.

               M := Make_Attribute_Reference (Ada_Enum_Type, A_Pos);
               M := Make_Subprogram_Call (M, New_List (N));
               N := Make_Type_Conversion (RE (RE_Unsigned_Long_10), M);
            end;

         when K_String =>
            begin
               if FEN.Kind (Var_Type) /= K_String then
                  N := Make_Type_Conversion (RE (RE_String_0), N);
               end if;

               N := Make_Subprogram_Call
                 (RE (RE_To_Standard_String),
                  New_List (N));
            end;

         when K_String_Type =>
            declare
               Str_Package_Node : Node_Id;
               Str_Type         : Node_Id;
               Str_Convert_Subp : Node_Id;
            begin

               --  Getting the instantiated package node

               Str_Package_Node := Defining_Identifier
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Getting the conversion subprogram

               Str_Type := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (TN (T_Bounded_String)));

               Str_Convert_Subp := Make_Selected_Component
                 (Str_Package_Node,
                  Make_Identifier (SN (S_To_String)));

               N := Make_Type_Conversion (Str_Type, N);

               N := Make_Subprogram_Call
                 (Str_Convert_Subp,
                  New_List (N));

               N := Make_Subprogram_Call
                 (RE (RE_To_PolyORB_String),
                  New_List (N));
            end;

         when K_Sequence_Type =>
            declare
               Seq_Package_Node : Node_Id;
               Seq_Type         : Node_Id;
            begin
               --  Getting the instantiated package node in aligned
               --  backend.

               Seq_Package_Node := Expand_Designator
                 (Instantiation_Node (BE_Node (Orig_Type)));

               --  Sequence type

               Seq_Type := Make_Selected_Component
                 (Seq_Package_Node,
                  Make_Identifier (TN (T_Sequence)));

               N := Make_Type_Conversion (Seq_Type, N);
            end;

         when others =>
            null;
      end case;

      return N;
   end Cast_Variable_To_PolyORB_Aligned_Type;

   -------------------
   -- Marshall_Args --
   -------------------

   procedure Marshall_Args
     (Stat     : List_Id;
      Var_Type : Node_Id;
      Var      : Node_Id;
      Var_Exp  : Node_Id := No_Node)
   is
      Rewinded_Type : Node_Id;
      C             : Node_Id;
      N             : Node_Id;
      M             : Node_Id;
   begin
      Rewinded_Type := FEU.Get_Original_Type_Specifier (Var_Type);

      case FEN.Kind (Rewinded_Type) is
         when K_Structure_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Members (Rewinded_Type));
               while Present (Member) loop
                  C := Make_Selected_Component
                    (Var,
                     Make_Identifier
                     (IDL_Name
                      (Identifier
                       (First_Entity
                        (Declarators
                         (Member))))));

                  if Var_Exp /= No_Node then
                     M := Make_Selected_Component
                       (Var_Exp,
                        Make_Identifier
                        (IDL_Name
                         (Identifier
                          (First_Entity
                           (Declarators
                            (Member))))));
                     Marshall_Args (Stat, Type_Spec (Member), C, M);
                  else
                     Marshall_Args (Stat, Type_Spec (Member), C);
                  end if;
                  Member := Next_Entity (Member);
               end loop;
               return;
            end;

         when K_Union_Type =>
            declare
               L                   : List_Id;
               Literal_Parent      : Node_Id := No_Node;
               Choices             : List_Id;
               Switch_Alternatives : List_Id;
               Switch_Node         : Node_Id;
               Switch_Case         : Node_Id;
               Has_Default         : Boolean := False;
            begin
               Switch_Node :=
                 Make_Identifier (FEN.Switch_Name (Rewinded_Type));
               if Var_Exp /= No_Node then
                  Switch_Node := Make_Selected_Component
                    (Var_Exp, Switch_Node);
               else
                  Switch_Node := Make_Selected_Component (Var, Switch_Node);
               end if;

               C := FEU.Get_Original_Type_Specifier
                 (Switch_Type_Spec (Rewinded_Type));

               if FEN.Kind (C) = K_Enumeration_Type then
                  Literal_Parent := Map_Expanded_Name
                    (Scope_Entity (Identifier (C)));
               end if;

               Switch_Alternatives := New_List;
               Switch_Case := First_Entity (Switch_Type_Body (Rewinded_Type));

               while Present (Switch_Case) loop
                  Map_Choice_List
                    (Labels (Switch_Case),
                     Literal_Parent,
                     Choices,
                     Has_Default);

                  L := New_List;

                  C := Make_Selected_Component
                    (Var,
                     Make_Identifier
                     (IDL_Name
                      (Identifier
                       (Declarator
                        (Element (Switch_Case))))));

                  if Var_Exp /= No_Node then
                     M := Make_Selected_Component
                       (Var_Exp,
                        Make_Identifier
                        (IDL_Name
                         (Identifier
                          (Declarator
                           (Element (Switch_Case))))));
                     Marshall_Args
                       (L, Type_Spec (Element (Switch_Case)), C, M);
                  else
                     Marshall_Args (L, Type_Spec (Element (Switch_Case)), C);
                  end if;

                  --  Building the switch alternative

                  Append_To (Switch_Alternatives,
                    Make_Case_Statement_Alternative (Choices, L));

                  Switch_Case := Next_Entity (Switch_Case);
               end loop;

               --  Add an empty when others clause to keep the compiler happy

               if not Has_Default then
                  Append_To (Switch_Alternatives,
                    Make_Case_Statement_Alternative (No_List, No_List));
               end if;

               N := Make_Case_Statement
                 (Switch_Node,
                  Switch_Alternatives);
               Append_To (Stat, N);
               return;
            end;

         when K_String =>
            if Var_Exp /= No_Node then
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var_Exp, Var_Type);
            else
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            end if;

            C := RE (RE_Nul);
            M := Make_Expression (M, Op_And_Symbol, C);
            C := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            N := Make_Selected_Component
              (PN (P_Content),
               Fully_Qualified_Name (Var));

            if Var_Exp /= No_Node then
               N := Make_Selected_Component
                 (VN (V_Args_Out), Fully_Qualified_Name (N));
            else
               N := Make_Selected_Component
                 (VN (V_Args_In), Fully_Qualified_Name (N));
            end if;

            N := Make_Assignment_Statement (N, M);
            Append_To (Stat, N);
            return;

         when K_Sequence_Type =>
            declare
               Range_Constraint : Node_Id;
               Index_Node       : Node_Id;
               K                : Node_Id;
            begin
               Set_Str_To_Name_Buffer ("J");
               Index_Node := Make_Defining_Identifier (Name_Find);

               if Var_Exp /= No_Node then
                  N := Make_Subprogram_Call
                    (RE (RE_Length_2), New_List (Var_Exp));
               else
                  N := Make_Subprogram_Call
                    (RE (RE_Length_2), New_List (Var));
               end if;

               N := Make_Type_Conversion (RE (RE_Unsigned_Long_10), N);

               Range_Constraint := Make_Range_Constraint
                 (Make_Literal (Int1_Val), N);

               N := Make_Selected_Component
                 (PN (P_Content),
                  Fully_Qualified_Name (Var));

               if Var_Exp /= No_Node then
                  N := Make_Selected_Component
                    (VN (V_Args_Out), Fully_Qualified_Name (N));
                  N := Make_Identifier (Fully_Qualified_Name (N));
                  N := Make_Subprogram_Call
                    (N, New_List (Index_Node));

                  M := Make_Identifier (Fully_Qualified_Name (Var_Exp));
                  K := Make_Type_Conversion (RE (RE_Integer), Index_Node);
                  M := Make_Subprogram_Call
                    (RE (RE_Get_Element), New_List (M, K));

                  M := Cast_Variable_To_PolyORB_Aligned_Type
                    (M,
                     Type_Spec
                     (Type_Spec
                      (Declaration (Reference (Var_Type)))));

                  N := Make_Assignment_Statement (N, M);
               else
                  N := Make_Selected_Component
                    (VN (V_Args_In), Fully_Qualified_Name (N));
                  N := Make_Identifier (Fully_Qualified_Name (N));
                  N := Make_Subprogram_Call
                    (N, New_List (Index_Node));

                  M := Make_Identifier (Fully_Qualified_Name (Var));
                  K := Make_Type_Conversion (RE (RE_Integer), Index_Node);
                  M := Make_Subprogram_Call
                    (RE (RE_Get_Element), New_List (M, K));

                  M := Cast_Variable_To_PolyORB_Aligned_Type
                    (M,
                     Type_Spec
                     (Type_Spec
                      (Declaration (Reference (Var_Type)))));

                  N := Make_Assignment_Statement (N, M);
               end if;

               N := Make_For_Statement
                 (Index_Node, Range_Constraint, New_List (N));
               Append_To (Stat, N);

               return;
            end;

         when K_Complex_Declarator =>
            M := Make_Selected_Component
              (Defining_Identifier (Aligned_Package (Current_Entity)),
               Make_Identifier (IDL_Name (Identifier (Var_Type))));
            M := Make_Subprogram_Call (M, New_List (Var));

         when others =>
            if Var_Exp /= No_Node then
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var_Exp, Var_Type);
            else
               M := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
            end if;
            C := Cast_Variable_To_PolyORB_Aligned_Type (Var, Var_Type);
      end case;

      N := Make_Identifier (Fully_Qualified_Name (Var));

      if Var_Exp /= No_Node then
         N := Make_Selected_Component
           (VN (V_Args_Out), Fully_Qualified_Name (Var));
      else
         N := Make_Selected_Component
           (VN (V_Args_Out), Fully_Qualified_Name (Var));
      end if;

      N := Make_Assignment_Statement (N, M);
      Append_To (Stat, N);
   end Marshall_Args;

   -----------------------------
   -- Get_Discriminants_Value --
   -----------------------------

   procedure Get_Discriminants_Value (P      : Node_Id;
                                      N      : Node_Id;
                                      L      : List_Id;
                                      Ret    : Boolean := False)
   is
      Rewinded_Type : Node_Id;
      Var           : Node_Id;
      M             : Node_Id;
      C             : Node_Id;
   begin
      --  Handle the case of non void operation having OUT parameters

      if FEN.Kind (P) = K_Parameter_Declaration then
         Var := Map_Defining_Identifier (Declarator (P));
      else
         Var := Make_Defining_Identifier (PN (P_Returns));
      end if;

      Rewinded_Type := FEU.Get_Original_Type_Specifier (N);

      case FEN.Kind (Rewinded_Type) is
         when K_Union_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Switch_Type_Body (Rewinded_Type));

               while Present (Member) loop
                  M := Make_Selected_Component
                    (Var,
                     Make_Defining_Identifier
                     (IDL_Name
                      (Identifier
                       (Declarator
                        (Element
                         (Member))))));
                  Get_Discriminants_Value
                    (M, Type_Spec (Element (Member)), L);
                  Member := Next_Entity (Member);
               end loop;

               M := Make_Selected_Component
                 (Var,
                  Make_Identifier (FEN.Switch_Name (Rewinded_Type)));

               C := Switch_Type_Spec (Rewinded_Type);
               M := Cast_Variable_To_PolyORB_Aligned_Type (M, C);
               Append_To (L, M);
            end;

         when K_Structure_Type =>
            declare
               Member : Node_Id;
            begin
               Member := First_Entity (Members (Rewinded_Type));

               while Present (Member) loop
                  M := Make_Selected_Component
                    (Var,
                     Make_Identifier
                     (IDL_Name
                      (Identifier
                       (First_Entity
                        (Declarators
                         (Member))))));
                  Get_Discriminants_Value (M, Type_Spec (Member), L, Ret);
                  Member := Next_Entity (Member);
               end loop;
            end;

         when K_String | K_Wide_String =>
            C := Make_Attribute_Reference
              (Make_Subprogram_Call
               (RE (RE_To_Standard_String), New_List (Var)),
               A_Length);
            C := Make_Expression
              (C, Op_Plus, Make_Literal (Values.New_Integer_Value (1, 1, 10)));
            Append_To (L, C);

         when K_Sequence_Type =>

            if not Present (Max_Size (Rewinded_Type)) then
               C := Make_Subprogram_Call
                 (RE (RE_Length_2), New_List (Var));
               C := Make_Type_Conversion (RE (RE_Unsigned_Long_10), C);
               Append_To (L, C);
            end if;

         when others =>
            null;
      end case;
   end Get_Discriminants_Value;
end Backend.BE_CORBA_Ada.Common;
