------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2021, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.GMP.Integers;

with Langkit_Support.Adalog.Debug;
with Langkit_Support.Bump_Ptr;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Doc_Utils;
with Libadalang.Env_Hooks;
with Libadalang.Expr_Eval;
with Libadalang.Public_Converters;
with Libadalang.Sources;
with Libadalang.Unit_Files;

--  Extension to store the code for external properties

package body Libadalang.Implementation.Extensions is

   procedure Alloc_Logic_Vars (Node : Bare_Expr) with Inline;

   -------------------------
   -- Ada_Node_P_Get_Unit --
   -------------------------

   function Ada_Node_P_Get_Unit
     (Node           : Bare_Ada_Node;
      Name           : Symbol_Type_Array_Access;
      Kind           : Analysis_Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit is
   begin
      return Env_Hooks.Fetch_Unit
        (Node.Unit.Context,
         Env_Hooks.Symbol_Type_Array (Name.Items),
         Node.Unit,
         Kind,
         Load_If_Needed);
   end Ada_Node_P_Get_Unit;

   ------------------------------
   -- Ada_Node_P_Standard_Unit --
   ------------------------------

   function Ada_Node_P_Standard_Unit
     (Node : Bare_Ada_Node) return Internal_Unit is
   begin
      return Get_From_File
        (Context     => Node.Unit.Context,
         Filename    => "__standard",
         Charset     => "",
         Reparse     => False,
         Rule        => Default_Grammar_Rule);
   end Ada_Node_P_Standard_Unit;

   --------------------------------------
   -- Ada_Node_P_Filter_Is_Imported_By --
   --------------------------------------

   function Ada_Node_P_Filter_Is_Imported_By
     (Node       : Bare_Ada_Node;
      Units      : Internal_Unit_Array_Access;
      Transitive : Boolean) return Internal_Unit_Array_Access
   is

      package Analysis_Unit_Vectors is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => Internal_Unit);

      package Analysis_Unit_Maps is new Ada.Containers.Hashed_Maps
        (Key_Type        => Internal_Unit,
         Element_Type    => Boolean,
         Hash            => Hash,
         Equivalent_Keys => "=");

      type CU_Array is array (Positive range <>) of Bare_Compilation_Unit;

      Context : constant Internal_Context := Node.Unit.Context;

      Ada_Text_IO_Symbol_Array : constant Internal_Symbol_Type_Array :=
        (1 => Lookup_Symbol (Context, "ada"),
         2 => Lookup_Symbol (Context, "text_io"));

      Ada_Text_IO_Special_Packages : constant Internal_Symbol_Type_Array :=
        (Lookup_Symbol (Context, "integer_io"),
         Lookup_Symbol (Context, "modular_io"),
         Lookup_Symbol (Context, "float_io"),
         Lookup_Symbol (Context, "fixed_io"),
         Lookup_Symbol (Context, "decimal_io"),
         Lookup_Symbol (Context, "enumeration_io"));

      function Is_Special_Unit_Name
        (Symbols : Internal_Symbol_Type_Array) return Boolean;
      --  Return True iff the given name (as an array of symbols) matches
      --  the name of one of those special units:
      --  Ada.Text_IO.{Integer_IO, Modular_IO, Float_IO, Fixed_IO,
      --               Decimal_IO, Enumeration_IO}
      --  See `Actual_Target` to find out why these are considered special.

      function Actual_Target return Internal_Unit;
      --  There are special packages in the Ada runtime that are handled in an
      --  ad-hoc way by GNAT: they are viewed as nested packages of Ada.Text_IO
      --  by the users but are actually written as *child* packages of
      --  Ada.Text_IO.
      --
      --  This procedure checks whether the given target is not one of those
      --  and if it is, it modifies the target so it points to the unit
      --  defining Ada.Text_IO instead, which allows the correct behavior of
      --  this whole routine.

      function All_Compilation_Units_From
        (Root : Bare_Ada_Node) return CU_Array;
      --  Given a node that is the root of an analysis unit, return all the
      --  compilation units that are defined inside of it.

      function Does_Import_Target (From : Internal_Unit) return Boolean;
      --  Predicate that returns True iff the given unit imports the target.
      --  If Transitive is True, handle transitive imports.

      --------------------------
      -- Is_Special_Unit_Name --
      --------------------------

      function Is_Special_Unit_Name
        (Symbols : Internal_Symbol_Type_Array) return Boolean is
      begin
         --  Check that `Symbols` is of the form ("ada", "text_io", "xxx_io")
         --  with xxx_io being the name of one of the special packages.
         return Symbols'Length = 3
                and then Symbols (1 .. 2) = Ada_Text_IO_Symbol_Array
                and then (for some Special_Package_Name
                          of Ada_Text_IO_Special_Packages =>
                          Symbols (3) = Special_Package_Name);
      end Is_Special_Unit_Name;

      -------------------
      -- Actual_Target --
      -------------------

      function Actual_Target return Internal_Unit is
         Target : constant Internal_Unit := Node.Unit;
         T_Root : constant Bare_Ada_Node := Root (Target);
      begin
         if T_Root /= null and then T_Root.Kind = Ada_Compilation_Unit then
            declare
               Qualified_Name : Symbol_Type_Array_Access :=
                  Compilation_Unit_P_Syntactic_Fully_Qualified_Name (T_Root);
               Is_Special     : constant Boolean :=
                  Is_Special_Unit_Name (Qualified_Name.Items);
            begin
               Dec_Ref (Qualified_Name);
               if Is_Special then
                  return Env_Hooks.Fetch_Unit
                    (Context,
                     Env_Hooks.Symbol_Type_Array (Ada_Text_IO_Symbol_Array),
                     Node.Unit, Unit_Specification, True, False);
               end if;
            end;
         end if;
         return Target;
      end Actual_Target;

      ---------------------------
      -- All_Compilation_Units --
      ---------------------------

      function All_Compilation_Units_From
        (Root : Bare_Ada_Node) return CU_Array is
      begin
         if Root = null then
            return (1 .. 0 => <>);
         end if;

         case Unit_Files.Root_Nodes (Root.Kind) is
            when Ada_Compilation_Unit =>
               return (1 => Bare_Compilation_Unit (Root));
            when Ada_Compilation_Unit_List =>
               declare
                  List : constant Bare_Compilation_Unit_List :=
                     Bare_Compilation_Unit_List (Root);

                  Res : CU_Array (1 .. List.Count);
               begin
                  for I in 1 .. List.Count loop
                     Res (I) := List.Nodes (I);
                  end loop;
                  return Res;
               end;
            when Ada_Pragma_Node_List =>
               return (1 .. 0 => <>);
         end case;
      end All_Compilation_Units_From;

      Target              : constant Internal_Unit := Actual_Target;
      Units_Import_Target : Analysis_Unit_Maps.Map;

      ------------------------
      -- Does_Import_Target --
      ------------------------

      function Does_Import_Target (From : Internal_Unit) return Boolean is
         Root_Node   : constant Bare_Ada_Node := Root (From);
         Units       : constant CU_Array :=
            All_Compilation_Units_From (Root_Node);

         From_Cursor : constant Analysis_Unit_Maps.Cursor :=
            Units_Import_Target.Find (From);
      begin
         if Analysis_Unit_Maps.Has_Element (From_Cursor) then
            return Analysis_Unit_Maps.Element (From_Cursor);
         elsif Root_Node = null then
            return False;
         end if;

         if From = Target then
            Units_Import_Target.Insert (From, True);
            return True;
         end if;

         Units_Import_Target.Insert (From, False);

         for Comp_Unit of Units loop
            declare
               Units_Array : Internal_Entity_Compilation_Unit_Array_Access :=
                  Compilation_Unit_P_Imported_Units (Comp_Unit);
            begin
               for Imported_Unit of Units_Array.Items loop
                  if Imported_Unit.Node /= null then
                     declare
                        Unit : constant Internal_Unit :=
                           Imported_Unit.Node.Unit;
                     begin
                        if Unit = Target or else
                             (Transitive and then Does_Import_Target (Unit))
                        then
                           Units_Import_Target.Replace (From, True);
                           Dec_Ref (Units_Array);
                           return True;
                        end if;
                     end;
                  end if;
               end loop;
               Dec_Ref (Units_Array);
            end;
         end loop;

         return False;
      end Does_Import_Target;

      Result_Vector : Analysis_Unit_Vectors.Vector;
   begin
      --  Place the units that satisfy the predicate into a temporary vector
      for Unit of Units.Items loop
         if Does_Import_Target (Unit) then
            Result_Vector.Append (Unit);
         end if;
      end loop;

      --  Create the result array from the vector
      declare
         Result : constant Internal_Unit_Array_Access :=
            Create_Internal_Unit_Array (Natural (Result_Vector.Length));
         N      : Positive := Result.Items'First;
      begin
         for Unit of Result_Vector loop
            Result.Items (N) := Unit;
            N := N + 1;
         end loop;
         return Result;
      end;
   end Ada_Node_P_Filter_Is_Imported_By;

   -------------------------
   -- Base_Id_Short_Image --
   -------------------------

   function Base_Id_Short_Image (Node : Bare_Base_Id) return Text_Type is
   begin
      return
        "<" & To_Text (Kind_Name (Node))
        & " """ & Text (Node) & """ "
        & To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))))
        & ":" & To_Text (Image (Sloc_Range (Node))) & ">";
   end Base_Id_Short_Image;

   ----------------------------
   -- Basic_Decl_Short_Image --
   ----------------------------

   function Basic_Decl_Short_Image (Node : Bare_Basic_Decl) return Text_Type is
      use Ada.Strings.Wide_Wide_Unbounded;

      Ret : Unbounded_Wide_Wide_String;

      function Name_Image (Node : Bare_Expr) return Text_Type;
      --  Print a Dotted Name

      ----------------
      -- Name_Image --
      ----------------

      function Name_Image (Node : Bare_Expr) return Text_Type is
      begin
         if Node = null then
            return "None";
         end if;

         case Env_Hooks.Defining_Name_Nodes (Node.Kind) is
            when Ada_Base_Id =>
               return Text (Node);

            when Ada_Dotted_Name =>
               return (Name_Image (Node.Dotted_Name_F_Prefix)
                       & "." & Name_Image (Node.Dotted_Name_F_Suffix));

            when Ada_Defining_Name =>
               return Name_Image (Node.Defining_Name_F_Name);
         end case;
      end Name_Image;

      Children : Internal_Entity_Defining_Name_Array_Access :=
         Dispatcher_Basic_Decl_P_Defining_Names (Node);
   begin
      Append (Ret, "<" & To_Text (Kind_Name (Node)) & " [");

      for I in Children.Items'Range loop
         Append (Ret, """");
         Append
           (Ret,
            Name_Image (Children.Items (I).Node));
         Append (Ret, """");
         if I /= Children.Items'Last then
            Append (Ret, ", ");
         end if;
      end loop;
      Dec_Ref (Children);

      Append
        (Ret,
         "] "
         & To_Text (Ada.Directories.Simple_Name (Get_Filename (Node.Unit)))
         & ":" & To_Text (Image (Sloc_Range (Node))) & ">");

      return To_Wide_Wide_String (Ret);
   end Basic_Decl_Short_Image;

   ----------------------
   -- Basic_Decl_P_Doc --
   ----------------------

   function Basic_Decl_P_Doc
     (Node : Bare_Basic_Decl) return Character_Type_Array_Access
   is
      use Libadalang.Doc_Utils;
      Decl : constant Libadalang.Analysis.Basic_Decl :=
         Public_Converters.Wrap_Node (Node).As_Basic_Decl;
      Doc  : constant Doc_Type := Get_Documentation (Decl);
      Ret  : constant Character_Type_Array_Access :=
         Create_Character_Type_Array (Doc.Doc.Length);
   begin
      Ret.Items := Doc.Doc.To_String;
      return Ret;
   end Basic_Decl_P_Doc;

   ----------------------------------
   -- Basic_Decl_P_Doc_Annotations --
   ----------------------------------

   function Basic_Decl_P_Doc_Annotations
     (Node : Bare_Basic_Decl) return Internal_Doc_Annotation_Array_Access
   is
      use Libadalang.Doc_Utils;
      Decl : constant Basic_Decl :=
         Public_Converters.Wrap_Node (Node).As_Basic_Decl;
      Doc  : constant Doc_Type := Get_Documentation (Decl);
      Ret  : constant Internal_Doc_Annotation_Array_Access :=
         Create_Internal_Doc_Annotation_Array
           (Natural (Doc.Annotations.Length));
      Idx  : Positive := 1;
   begin
      for El in Doc.Annotations.Iterate loop
         declare
            Key     : constant Text_Type := Annotations_Maps.Key (El);
            Val     : constant Text_Type := Annotations_Maps.Element (El);
            DSL_Key : constant Character_Type_Array_Access :=
              Create_Character_Type_Array (Key'Length);
            DSL_Val : constant Character_Type_Array_Access :=
              Create_Character_Type_Array (Val'Length);
         begin
            DSL_Key.Items := Key;
            DSL_Val.Items := Val;
            Ret.Items (Idx) := (Key => DSL_Key, Value => DSL_Val);
         end;
         Idx := Idx + 1;
      end loop;
      return Ret;
   end Basic_Decl_P_Doc_Annotations;

   ----------------------------------
   -- Char_Literal_P_Denoted_Value --
   ----------------------------------

   function Char_Literal_P_Denoted_Value
     (Node : Bare_Char_Literal) return Character_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return Sources.Decode_Character_Literal (N_Text);
   end Char_Literal_P_Denoted_Value;

   --------------------------------------
   -- Compilation_Unit_P_Get_Empty_Env --
   --------------------------------------

   function Compilation_Unit_P_Get_Empty_Env
     (Node : Bare_Compilation_Unit) return Lexical_Env is
   begin
      if Node.Compilation_Unit_No_Env = Empty_Env then
         Node.Compilation_Unit_No_Env :=
            Create_Static_Lexical_Env (No_Env_Getter, Node);
      end if;
      return Node.Compilation_Unit_No_Env;
   end Compilation_Unit_P_Get_Empty_Env;

   ----------------------
   -- Expr_Eval_In_Env --
   ----------------------

   package Eval renames Libadalang.Expr_Eval;

   function Expr_Eval_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Eval.Eval_Result;

   function Expr_Eval_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Eval.Eval_Result
   is
      N : constant Expr := Public_Converters.Wrap_Node (Node, E_Info).As_Expr;
      E : Substitution_Array (Env.Items'First .. Env.Items'Last);
   begin
      for I in Env.Items'Range loop
         declare
            Bare_Subst : constant Internal_Substitution := Env.Items (I);
         begin
            E (I) := Create_Substitution
              (From_Decl => Public_Converters.Wrap_Node
                 (Bare_Subst.From_Decl.Node).As_Basic_Decl,
               Value_Type => Public_Converters.Wrap_Node
                 (Bare_Subst.Value_Type.Node).As_Base_Type_Decl,
               To_Value  => Bare_Subst.To_Value.Value);
         end;
      end loop;
      return Eval.Expr_Eval_In_Env (N, E);
   end Expr_Eval_In_Env;

   -------------------------------
   -- Expr_P_Eval_As_Int_In_Env --
   -------------------------------

   function Expr_P_Eval_As_Int_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Big_Integer_Type
   is
   begin
      return Create_Big_Integer
         (Eval.As_Int (Expr_Eval_In_Env (Node, Env, E_Info)));
   end Expr_P_Eval_As_Int_In_Env;

   ----------------------------------
   -- Expr_P_Eval_As_String_In_Env --
   ----------------------------------

   function Expr_P_Eval_As_String_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Character_Type_Array_Access
   is
   begin
      return Create_Character_Type_Array
         (Eval.As_String (Expr_Eval_In_Env (Node, Env, E_Info)));
   end Expr_P_Eval_As_String_In_Env;

   -----------------------------------------------
   -- Generic_Instantiation_P_Instantiation_Env --
   -----------------------------------------------

   function Generic_Instantiation_P_Instantiation_Env
     (Node : Bare_Generic_Instantiation) return Lexical_Env is
   begin
      if Node.Generic_Instantiation_Inst_Env = Empty_Env then
         Node.Generic_Instantiation_Inst_Env := Create_Static_Lexical_Env
           (No_Env_Getter, Node);
      end if;
      return Node.Generic_Instantiation_Inst_Env;
   end Generic_Instantiation_P_Instantiation_Env;

   ---------------------------------
   -- Int_Literal_P_Denoted_Value --
   ---------------------------------

   function Int_Literal_P_Denoted_Value
     (Node : Bare_Int_Literal) return Big_Integer_Type
   is
      N_Text : constant Text_Type := Text (Node);
      Number : GNATCOLL.GMP.Integers.Big_Integer;
   begin
      Sources.Decode_Integer_Literal (N_Text, Number);
      return Create_Big_Integer (Number);
   end Int_Literal_P_Denoted_Value;

   -------------------------------------
   -- Name_P_Internal_Referenced_Unit --
   -------------------------------------

   function Name_P_Internal_Referenced_Unit
     (Node           : Bare_Name;
      Kind           : Analysis_Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit is
   begin
      return Env_Hooks.Fetch_Unit
        (Node.Unit.Context, Node, Kind, Load_If_Needed);
   exception
      when Property_Error =>
         return No_Analysis_Unit;
   end Name_P_Internal_Referenced_Unit;

   ------------------------------------
   -- String_Literal_P_Denoted_Value --
   ------------------------------------

   function String_Literal_P_Denoted_Value
     (Node : Bare_String_Literal) return Character_Type_Array_Access
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return Create_Character_Type_Array
        (Sources.Decode_String_Literal (N_Text));
   end String_Literal_P_Denoted_Value;

   package Alloc_Logic_Var_Array is new
     Langkit_Support.Bump_Ptr.Array_Alloc
       (Element_T  => Logic_Var_Record,
        Index_Type => Positive);

   ----------------------
   -- Alloc_Logic_Vars --
   ----------------------

   procedure Alloc_Logic_Vars (Node : Bare_Expr) is
   begin
      if Node.Expr_Logic_Vars = System.Null_Address then
         declare
            LV_Number : constant Positive :=
              (case Node.Kind is
                  when Ada_Single_Tok_Node => 3,
                  when others              => 1);

            Arr : constant Alloc_Logic_Var_Array.Element_Array_Access
              := Alloc_Logic_Var_Array.Alloc
                (Node.Unit.AST_Mem_Pool, LV_Number);
         begin
            for I in 1 .. LV_Number loop
               Arr (I) := Null_Var_Record;
            end loop;

            Node.Expr_Logic_Vars := Arr.all'Address;
         end;
      end if;
   end Alloc_Logic_Vars;

   -------------------------------
   -- Single_Tok_Node_P_Ref_Var --
   -------------------------------

   function Single_Tok_Node_P_Ref_Var
     (Node : Bare_Single_Tok_Node) return Logic_Var
   is
   begin
      Alloc_Logic_Vars (Node);

      return Ret : constant Logic_Var := Logic_Var'
        (Alloc_Logic_Var_Array.To_Pointer
           (Node.Expr_Logic_Vars) (2)'Unrestricted_Access)
      do
         if Langkit_Support.Adalog.Debug.Debug
            and then Ret.Dbg_Name = null
         then
            Ret.Dbg_Name := new String'
              (Image (Short_Text_Image (Node)) & "." & "P_Ref_Var");
         end if;
      end return;

   end Single_Tok_Node_P_Ref_Var;

   -------------------------------------
   -- Single_Tok_Node_P_Subp_Spec_Var --
   -------------------------------------

   function Single_Tok_Node_P_Subp_Spec_Var
     (Node : Bare_Single_Tok_Node) return Logic_Var
   is
   begin
      Alloc_Logic_Vars (Node);

      return Ret : constant Logic_Var := Logic_Var'
        (Alloc_Logic_Var_Array.To_Pointer
           (Node.Expr_Logic_Vars) (3)'Unrestricted_Access)
      do
         if Langkit_Support.Adalog.Debug.Debug
            and then Ret.Dbg_Name = null
         then
            Ret.Dbg_Name := new String'
              (Image (Short_Text_Image (Node)) & "." & "P_Subp_Spec_Var");
         end if;
      end return;
   end Single_Tok_Node_P_Subp_Spec_Var;

   ---------------------
   -- Expr_P_Type_Var --
   ---------------------

   function Expr_P_Type_Var (Node : Bare_Expr) return Logic_Var
   is
   begin
      Alloc_Logic_Vars (Node);

      return Ret : constant Logic_Var := Logic_Var'
        (Alloc_Logic_Var_Array.To_Pointer
           (Node.Expr_Logic_Vars) (1)'Unrestricted_Access)
      do
         if Langkit_Support.Adalog.Debug.Debug
            and then Ret.Dbg_Name = null
         then
            Ret.Dbg_Name := new String'
              (Image (Short_Text_Image (Node)) & "." & "P_Type_Var");
         end if;
      end return;
   end Expr_P_Type_Var;

   ----------------------------------
   -- Ada_Node_P_Resolve_Own_Names --
   ----------------------------------

   function Ada_Node_P_Resolve_Own_Names
     (Node   : Bare_Ada_Node;
      Env    : Lexical_Env;
      Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info) return Boolean
   is
      use Nameres_Maps;

      R : Relation;
      C : constant Cursor := Node.Unit.Nodes_Nameres.Find (Node);
   begin
      --  There was already resolution for this node, and it's the same
      --  rebindings, and the cache key is still fresh: just return
      --  existing result.

      if Nameres_Maps.Has_Element (C)
        and then Element (C).Cache_Version >= Node.Unit.Context.Cache_Version
        and then Nameres_Maps.Element (C).Rebindings = E_Info.Rebindings
      then
         declare
            Res_Val : constant Resolution_Val := Nameres_Maps.Element (C);
         begin
            if Res_Val.Raised_Exc then
               raise Property_Error with "Memoized Error";
            end if;

            return Nameres_Maps.Element (C).Return_Value;
         end;
      end if;

      R := Dispatcher_Ada_Node_P_Xref_Equation (Node, Env, Origin, E_Info);

      --  There was no resolution, or if there was it was for different
      --  rebindings. In that case, solve and include the result in the
      --  mmz map.
      return Res : constant Boolean := Solve_Wrapper (R,  Node) do
         Dec_Ref (R);
         Node.Unit.Nodes_Nameres.Include
           (Node,
            (Node.Unit.Context.Cache_Version, E_Info.Rebindings, Res, False));
      end return;

   exception
      when Property_Error =>
         Dec_Ref (R);
         --  Memoize the exception result, to be able to re-raise a
         --  property_error if this is called again with the same params.
         Node.Unit.Nodes_Nameres.Include
           (Node,
            (Node.Unit.Context.Cache_Version,
             E_Info.Rebindings, False, Raised_Exc => True));
         raise;
   end Ada_Node_P_Resolve_Own_Names;

end Libadalang.Implementation.Extensions;
