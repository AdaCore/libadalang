--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Wide_Wide_Unbounded;

with GNATCOLL.GMP.Integers;

with Langkit_Support.Adalog.Debug;
with Langkit_Support.Bump_Ptr;
with Langkit_Support.Symbols;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;
with Libadalang.Config_Pragmas_Impl;
with Libadalang.Doc_Utils;
with Libadalang.Env_Hooks;
with Libadalang.Expr_Eval;
with Libadalang.Public_Converters;
with Libadalang.Sources;
with Libadalang.Unit_Files;

--  Extension to store the code for external properties

package body Libadalang.Implementation.Extensions is

   procedure Alloc_Logic_Vars (Node : Bare_Expr) with Inline;

   function CU_Subunit (CU : Bare_Compilation_Unit) return Bare_Subunit;
   --  If CU is a subunit, return the corresponding subunit node. Return null
   --  otherwise.

   -----------------------
   -- Post_Parsing_Hook --
   -----------------------

   procedure Post_Parsing_Hook (Unit : Internal_Unit) is
      TDH : Token_Data_Handler renames Unit.Token_Data.all;

      ---------
      -- Add --
      ---------

      procedure Add (T : Stored_Token_Data);
      --  Assuming that T represents a preprocessing-specific token, add a
      --  diagnostic for it to Unit.Diagonstics.

      procedure Add (T : Stored_Token_Data) is
      begin
         Append
           (Unit.Diagnostics,
            Sloc_Range (TDH, T),
            "preprocessor directive ignored, preprocessor not active");
      end Add;

   begin
      if Unit.Context.Preprocessor_Directives_Errors then
         for T of TDH.Tokens loop
            if To_Token_Kind (T.Kind) = Ada_Identifier
               and then TDH.Source_Buffer (T.Source_First) = '$'
            then
               Add (T);
            end if;
         end loop;

         for T of TDH.Trivias loop
            if To_Token_Kind (T.T.Kind) = Ada_Prep_Line then
               Add (T.T);
            end if;
         end loop;
      end if;
   end Post_Parsing_Hook;

   ----------------
   -- CU_Subunit --
   ----------------

   function CU_Subunit (CU : Bare_Compilation_Unit) return Bare_Subunit is
      B : Bare_Ada_Node;
   begin
      if CU = null or else CU.Kind /= Ada_Compilation_Unit then
         return null;
      end if;
      B := CU.Compilation_Unit_F_Body;
      return (if B /= null and then B.Kind = Ada_Subunit
              then B
              else null);
   end CU_Subunit;

   --------------------------
   -- Ada_Node_P_Can_Reach --
   --------------------------

   function Ada_Node_P_Can_Reach
     (Node, From_Node : Bare_Ada_Node) return Boolean
   is
      function Can_Reach_In_Same_Unit
        (Node, From_Node : Bare_Ada_Node) return Boolean
      is (From_Node = null
          or else Node.Token_Start_Index < From_Node.Token_Start_Index);
      --  Assuming that ``Node`` and ``From_Node`` belong to the same unit,
      --  return whether ``From_Node`` can reach ``Node`` according to Ada's
      --  visibility rules: consider that From_Node has visibility over Node
      --  iff Node appears earlier in the token stream.
      --
      --  For convenience, assume that ``Node`` can be reached if ``From_Node``
      --  is null.

      Node_CU      : Bare_Compilation_Unit;
      From_CU      : Bare_Compilation_Unit;
      From_Subunit : Bare_Subunit;

   begin
      if Node = null then
         return True;
      end if;

      Node_CU := Ple_Root (Node);
      From_CU := Ple_Root (From_Node);

      --  If Node and From_Node belong to the same unit, just check their
      --  relative position.

      if Node_CU = From_CU then
         return Can_Reach_In_Same_Unit (Node, From_Node);
      end if;

      --  Otherwise, the only case for which From_Node *does not* have
      --  visibility over Node is when From_Node belongs to a subunit that is
      --  rooted in Node's unit, and the subunit stub appears before Node::
      --
      --    package body Foo is
      --      procedure Bar is separate;
      --      [Node]
      --    end Foo;
      --
      --    separate (Foo)
      --    procedure Bar is
      --      [From_Node]
      --    end Bar;
      --
      --  So first, check that From_Node belongs to a subunit.

      From_Subunit := CU_Subunit (From_CU);
      if From_Subunit = null then
         return True;
      end if;

      --  From_CU is a subunit, so climb the subunit/stub chain up until we
      --  find a stub in Node_CU, then apply the "same-unit" visibility rules.

      declare
         Stub : constant Bare_Body_Stub :=
           Compilation_Unit_P_Stub_For (Node_CU, From_Subunit);
      begin
         return Can_Reach_In_Same_Unit (Node, Stub);
      end;
   end Ada_Node_P_Can_Reach;

   -------------------------
   -- Ada_Node_P_Get_Unit --
   -------------------------

   function Ada_Node_P_Get_Unit
     (Node               : Bare_Ada_Node;
      Name               : Symbol_Type_Array_Access;
      Kind               : Analysis_Unit_Kind;
      Load_If_Needed     : Boolean;
      Not_Found_Is_Error : Boolean;
      Process_Parents    : Boolean := True) return Internal_Unit
   is
   begin
      return Env_Hooks.Fetch_Unit
        (Ctx                => Node.Unit.Context,
         Name               => Env_Hooks.Symbol_Type_Array (Name.Items),
         Kind               => Kind,
         From_Unit          => Node.Unit,
         Load_If_Needed     => Load_If_Needed,
         Not_Found_Is_Error => Not_Found_Is_Error,
         Process_Parents    => Process_Parents);
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

   ---------------------------
   -- Ada_Node_P_Is_Keyword --
   ---------------------------

   function Ada_Node_P_Is_Keyword
     (Node             : Bare_Ada_Node;
      Token            : Token_Reference;
      Language_Version : Symbol_Type) return Boolean
   is
      pragma Unreferenced (Node);

      Version_Text : constant String :=
        Langkit_Support.Symbols.Image (Language_Version);

      Version : Common.Language_Version;
   begin
      begin
         Version := Common.Language_Version'Value (Version_Text);
      exception
         when Constraint_Error =>
            raise Precondition_Failure
              with "Unknown Ada version " & Version_Text;
      end;
      return Is_Keyword (Token, Version);
   end Ada_Node_P_Is_Keyword;

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
        [1 => Lookup_Symbol (Context, "ada"),
         2 => Lookup_Symbol (Context, "text_io")];

      Ada_Text_IO_Special_Packages : constant Internal_Symbol_Type_Array :=
        [Lookup_Symbol (Context, "integer_io"),
         Lookup_Symbol (Context, "modular_io"),
         Lookup_Symbol (Context, "float_io"),
         Lookup_Symbol (Context, "fixed_io"),
         Lookup_Symbol (Context, "decimal_io"),
         Lookup_Symbol (Context, "enumeration_io")];

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
                    (Ctx                => Context,
                     Name               =>
                       Env_Hooks.Symbol_Type_Array (Ada_Text_IO_Symbol_Array),
                     Kind               => Unit_Specification,
                     From_Unit          => Node.Unit,
                     Load_If_Needed     => True,
                     Do_Prepare_Nameres => False);
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
            return [1 .. 0 => <>];
         end if;

         case Unit_Files.Root_Nodes (Root.Kind) is
            when Ada_Compilation_Unit =>
               return [Bare_Compilation_Unit (Root)];
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
               return [1 .. 0 => <>];
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

         case Node.Kind is
            when Ada_Base_Id =>
               return Text (Node);

            when Ada_Synthetic_Identifier =>
               return Image (Node.Synthetic_Identifier_Sym);

            when Ada_Dotted_Name =>
               return (Name_Image (Node.Dotted_Name_F_Prefix)
                       & "." & Name_Image (Node.Dotted_Name_F_Suffix));

            when Ada_Defining_Name_Range =>
               return Name_Image (Node.Defining_Name_F_Name);

            when others =>
               raise Constraint_Error with "Unexpected node kind";
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

   -------------------------------
   -- Defining_Name_Short_Image --
   -------------------------------

   function Defining_Name_Short_Image
     (Node : Bare_Defining_Name) return Text_Type
   is
      F_Name     : constant Bare_Name := Node.Defining_Name_F_Name;
      Name_Image : constant Text_Type :=
        (if F_Name.Kind = Ada_Synthetic_Identifier
         then +F_Name.Synthetic_Identifier_Sym
         else Text (Node));
      Name_Part  : constant Text_Type :=
        (if Name_Image = ""
         then " ??? "
         else " """ & Name_Image & """ ");
   begin
      return
        "<" & To_Text (Kind_Name (Node))
        & Name_Part
        & To_Text (Ada.Directories.Simple_Name (Get_Filename (Unit (Node))))
        & ":" & To_Text (Image (Sloc_Range (Node))) & ">";
   end Defining_Name_Short_Image;

   ----------------------
   -- Basic_Decl_P_Doc --
   ----------------------

   function Basic_Decl_P_Doc
     (Node : Bare_Basic_Decl) return String_Type
   is
      use Libadalang.Doc_Utils;
      Decl : constant Libadalang.Analysis.Basic_Decl :=
         Public_Converters.Wrap_Node (Node).As_Basic_Decl;
      Doc  : constant Doc_Type := Get_Documentation (Decl);
   begin
      return Create_String (Doc.Doc.To_String);
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
            DSL_Key : constant String_Type := Create_String (Key);
            DSL_Val : constant String_Type := Create_String (Val);
         begin
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
            Create_Static_Lexical_Env
              (Empty_Env, Node, Node.Unit.Context.Symbols);
      end if;
      return Node.Compilation_Unit_No_Env;
   end Compilation_Unit_P_Get_Empty_Env;

   ----------------------------------------
   -- Compilation_Unit_P_External_Config_Pragmas --
   ----------------------------------------

   function Compilation_Unit_P_External_Config_Pragmas
     (Node : Bare_Compilation_Unit) return Bare_Pragma_Node_Array_Access
   is

      function Fetch
        (Pragmas_Unit : Config_Pragmas_Impl.Internal_Unit)
         return Bare_Pragma_Node_List;
      --  Return the ``Pragma.list`` node that is the root of the
      --  ``Pragmas_Unit`` analysis unit, or null if there is no root node or
      --  if it is not a list of pragmas.

      procedure Append
        (Result  : in out Bare_Pragma_Node_Array_Record;
         Next    : in out Positive;
         Pragmas : Bare_Pragma_Node_List);
      --  Append all pragmas from ``Pragmas`` to ``Result`` starting at index
      --  ``Next``. Increment ``Next`` accordingly.

      -----------
      -- Fetch --
      -----------

      function Fetch
        (Pragmas_Unit : Config_Pragmas_Impl.Internal_Unit)
         return Bare_Pragma_Node_List
      is
         U : constant Internal_Unit := Internal_Unit (Pragmas_Unit);
      begin
         return
           (if U = null
               or else U.Ast_Root = null
               or else U.Ast_Root.Kind /= Ada_Pragma_Node_List
            then null
            else U.Ast_Root);
      end Fetch;

      ------------
      -- Append --
      ------------

      procedure Append
        (Result  : in out Bare_Pragma_Node_Array_Record;
         Next    : in out Positive;
         Pragmas : Bare_Pragma_Node_List) is
      begin
         if Pragmas = null then
            return;
         end if;

         for I in 1 .. Pragmas.Count loop
            Result.Items (Next) := Pragmas.Nodes (I);
            Next := Next + 1;
         end loop;
      end Append;

      use Config_Pragmas_Impl.Unit_Maps;

      U   : constant Internal_Unit := Node.Unit;
      Ctx : constant Internal_Context := U.Context;

   begin
      --  Fetch the global and local list of configuration pragmas (if any)

      declare
         Cur : constant Cursor := Ctx.Config_Pragmas.Local_Pragmas.Find
           (Config_Pragmas_Impl.Internal_Unit (U));

         Local  : constant Bare_Pragma_Node_List :=
           (if Has_Element (Cur)
            then Fetch (Element (Cur))
            else null);
         Global : constant Bare_Pragma_Node_List :=
           Fetch (Ctx.Config_Pragmas.Global_Pragmas);

         --  Now that we know how many pragmas are out there, allocate the
         --  result.

         Length : constant Natural :=
           (if Local = null then 0 else Local.Count)
           + (if Global = null then 0 else Global.Count);
         Next   : Positive := 1;
      begin
         return Result : constant Bare_Pragma_Node_Array_Access :=
           Create_Bare_Pragma_Node_Array (Length)
         do
            Append (Result.all, Next, Local);
            Append (Result.all, Next, Global);
         end return;
      end;
   end Compilation_Unit_P_External_Config_Pragmas;

   --------------------------------------
   -- Compilation_Unit_P_Stub_For_Impl --
   --------------------------------------

   function Compilation_Unit_P_Stub_For_Impl
     (Node : Bare_Compilation_Unit; Su : Bare_Subunit) return Bare_Body_Stub
   is
      CU      : Bare_Compilation_Unit := Ple_Root (Su);
      Subunit : Bare_Subunit := Su;
      --  Subunit that is currently inspected and its compilation unit

      Parent_CU : Bare_Compilation_Unit;
      --  Temporary to hold the parent compilation unit during one iteration
   begin
      loop
         --  Inspect the parent unit for CU/Subunit. If we cannot find it, the
         --  codebase is incomplete/invalid, so just return it could not be
         --  found.

         Parent_CU := Subunit_P_Root_Unit (Subunit).Node;
         if Parent_CU = null then
            return null;

         --  If the stub for CU/Subunit is supposed to belong to the same
         --  compilation unit as Node, fetch it and return it.

         elsif Parent_CU = Node then
            declare
               SU : constant Bare_Subunit := CU.Compilation_Unit_F_Body;
            begin
               return
                 (if SU = null
                  then null
                  else Subunit_P_Stub (SU));
            end;
         end if;

         --  No luck with Parent_CU: if it is a subunit itself, continue the
         --  search with its own parent unit.

         CU := Parent_CU;
         Subunit := CU_Subunit (CU);
         if Subunit = null then
            return null;
         end if;
      end loop;
   end Compilation_Unit_P_Stub_For_Impl;

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
      E_Info : Internal_Entity_Info) return String_Type
   is
   begin
      return Create_String
         (Eval.As_String (Expr_Eval_In_Env (Node, Env, E_Info)));
   end Expr_P_Eval_As_String_In_Env;

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

   ------------------------------------
   -- String_Literal_P_Denoted_Value --
   ------------------------------------

   function String_Literal_P_Denoted_Value
     (Node : Bare_String_Literal) return String_Type
   is
      N_Text : constant Text_Type := Text (Node);
   begin
      return Create_String (Sources.Decode_String_Literal (N_Text));
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
                  when Ada_Single_Tok_Node => 4,
                  when others              => 2);

            Arr : constant Alloc_Logic_Var_Array.Element_Array_Access
              := Alloc_Logic_Var_Array.Alloc
                (Node.Unit.Ast_Mem_Pool, LV_Number);
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
           (Node.Expr_Logic_Vars) (3)'Unrestricted_Access)
      do
         if Langkit_Support.Adalog.Debug.Debug
            and then Ret.Dbg_Name = null
         then
            Ret.Dbg_Name := New_Unit_String
              (Node.Unit, Image (Short_Text_Image (Node)) & "." & "P_Ref_Var");
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
           (Node.Expr_Logic_Vars) (4)'Unrestricted_Access)
      do
         if Langkit_Support.Adalog.Debug.Debug
            and then Ret.Dbg_Name = null
         then
            Ret.Dbg_Name := New_Unit_String
              (Node.Unit,
               Image (Short_Text_Image (Node)) & "." & "P_Subp_Spec_Var");
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
            Ret.Dbg_Name := New_Unit_String
              (Node.Unit,
               Image (Short_Text_Image (Node)) & "." & "P_Type_Var");
         end if;
      end return;
   end Expr_P_Type_Var;

   ------------------------------
   -- Expr_P_Expected_Type_Var --
   ------------------------------

   function Expr_P_Expected_Type_Var (Node : Bare_Expr) return Logic_Var
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
            Ret.Dbg_Name := New_Unit_String
              (Node.Unit,
               Image (Short_Text_Image (Node)) & "." & "P_Expected_Type_Var");
         end if;
      end return;
   end Expr_P_Expected_Type_Var;

   ----------------------------------
   -- Ada_Node_P_Resolve_Own_Names --
   ----------------------------------

   function Ada_Node_P_Resolve_Own_Names
     (Node                 : Bare_Ada_Node;
      Generate_Diagnostics : Boolean;
      Env                  : Lexical_Env;
      Origin               : Bare_Ada_Node;
      Entry_Point          : Bare_Ada_Node;
      E_Info               : Internal_Entity_Info := No_Entity_Info)
      return Boolean
   is
      use Nameres_Maps;
      use Libadalang.Implementation.Solver;

      Cache : Nameres_Maps.Map renames Node.Unit.Nodes_Nameres;
      C     : constant Cursor := Cache.Find (Node);
   begin
      --  If we already resolved this node with the same rebindings and if the
      --  cache is still fresh, return the memoized result.

      if Has_Element (C) then
         declare
            use type Ada.Exceptions.Exception_Id;
            Cached : Resolution_Val renames Cache.Reference (C);
         begin
            if Cached.Cache_Version >= Node.Unit.Context.Cache_Version
               and then Cached.Rebindings = E_Info.Rebindings
               and then (not Generate_Diagnostics
                         or else Cached.Has_Diagnostics)
            then
               if Cached.Exc_Id = Ada.Exceptions.Null_Id then
                  return Cached.Return_Value.Success;
               else
                  Reraise_Memoized_Error (Cached.Exc_Id, Cached.Exc_Msg);
               end if;
            end if;

            --  This cache entry will be replaced in the next code section no
            --  matter what, so decrease reference count here while we still
            --  have a reference to the cached value.
            Dec_Ref (Cached.Return_Value);
         end;
      end if;

      --  Past this point, we know we cannot rely on the cache: perform the
      --  resolution and memoize the result or the exception.

      declare
         R : Relation;
         V : Resolution_Val :=
           (Cache_Version   => Node.Unit.Context.Cache_Version,
            Rebindings      => E_Info.Rebindings,
            Has_Diagnostics => Generate_Diagnostics,
            Return_Value    => (False, null),
            Exc_Id          => Ada.Exceptions.Null_Id,
            Exc_Msg         => null);
      begin
         R := Dispatcher_Ada_Node_P_Xref_Equation
           (Node, Env, Origin, Entry_Point, E_Info);

         if Generate_Diagnostics then
            V.Return_Value := Solve_With_Diagnostics (R, Node);
         else
            V.Return_Value.Success := Solve_Wrapper (R, Node);
            V.Return_Value.Diagnostics :=
              Create_Internal_Solver_Diagnostic_Array (0);
         end if;
         Dec_Ref (R);

         Cache.Include (Node, V);
         return V.Return_Value.Success;
      exception
         when Exc : others =>
            if Properties_May_Raise (Exc) then
               Dec_Ref (R);
               Store_Memoized_Error (Exc, V.Exc_Id, V.Exc_Msg);
               Cache.Include (Node, V);
            end if;
            raise;
      end;
   end Ada_Node_P_Resolve_Own_Names;

   ----------------------------------------
   -- Ada_Node_P_Own_Nameres_Diagnostics --
   ----------------------------------------

   function Ada_Node_P_Own_Nameres_Diagnostics
     (Node                 : Bare_Ada_Node;
      E_Info               : Internal_Entity_Info := No_Entity_Info)
      return Internal_Solver_Diagnostic_Array_Access
   is
      use Nameres_Maps;

      Cache : Nameres_Maps.Map renames Node.Unit.Nodes_Nameres;
      C     : constant Cursor := Cache.Find (Node);
   begin
      if Has_Element (C) then
         declare
            use type Ada.Exceptions.Exception_Id;
            Cached : Resolution_Val renames Cache.Reference (C);
         begin
            if Cached.Cache_Version >= Node.Unit.Context.Cache_Version
               and then Cached.Rebindings = E_Info.Rebindings
               and then Cached.Has_Diagnostics
            then
               if Cached.Exc_Id = Ada.Exceptions.Null_Id then
                  Inc_Ref (Cached.Return_Value.Diagnostics);
                  return Cached.Return_Value.Diagnostics;
               else
                  Reraise_Memoized_Error (Cached.Exc_Id, Cached.Exc_Msg);
               end if;
            end if;
         end;
      end if;

      return Create_Internal_Solver_Diagnostic_Array (0);
   end Ada_Node_P_Own_Nameres_Diagnostics;

   -------------------------------
   -- Should_Collect_Env_Caches --
   -------------------------------

   function Should_Collect_Env_Caches
     (Ctx                        : Internal_Context;
      Unit                       : Internal_Unit;
      All_Env_Caches_Entry_Count : Long_Long_Natural) return Boolean
   is
      Ctx_Stats  : Context_Env_Caches_Stats renames Ctx.Env_Caches_Stats;
      Unit_Stats : Unit_Env_Caches_Stats renames Unit.Env_Caches_Stats;
   begin
      --  We only consider units which hold a minimal amount of cache
      --  entries, to avoid wasting cycles collecting the same seldom-
      --  used units which don't take much memory.
      if Unit_Stats.Entry_Count < 100 then
         return False;
      end if;

      declare
         Hit_Ratio : constant Float :=
           (if Unit_Stats.Lookup_Count = 0 then 1.0
            else Float (Unit_Stats.Hit_Count)
                 / Float (Unit_Stats.Lookup_Count));
         --  Ratio of cache hits over total cache lookups since this unit was
         --  last collected.

         Lookup_Ratio : constant Float :=
           Float (Unit_Stats.Lookup_Count)
           / Float (Ctx_Stats.Lookup_Count
                    - Unit_Stats.Last_Overall_Lookup_Count);
         --  Ratio of lookups done on this unit over total lookups done on any
         --  unit since this unit was last collected.

         Recent_Lookup_Ratio : constant Float :=
           Float (Unit_Stats.Lookup_Count
                  - Unit_Stats.Previous_Lookup_Count)
           / Float (Ctx_Stats.Lookup_Count
                    - Ctx_Stats.Previous_Lookup_Count);
         --  Ratio of lookups done on this unit over total lookups done on any
         --  unit since last time a collection was *attempted*.

         Entry_Ratio : constant Float :=
           Float (Unit_Stats.Entry_Count)
           / Float (All_Env_Caches_Entry_Count);
         --  Ratio of cache entries stored in this unit over total number of
         --  cache entries spread across all units.

         Usefulness_Score : constant Float :=
           Lookup_Ratio * Hit_Ratio * (2.0 + 5.0 * Recent_Lookup_Ratio);
         --  Score to estimate how useful the cache entries in this unit are:
         --  0 means that the cache entries are useless (we want to get rid of
         --  them), and a score greater than Entry_Ratio implies that we want
         --  to keep caches entries.
         --
         --  How to compute this score was deduced from trial and error, here
         --  is how it is supposed to work:
         --
         --  * 0 means that this cache is useless because we haven't looked up
         --    any of its entries. This is why Lookup_Ratio should be a
         --    multiplicative factor for the overall expression.
         --
         --  * If Hit_Ratio is 0, this cache is useless because even if we
         --    looked up its entries, we never found a relevant one. So
         --    Hit_Ratio must be a multiplicative factor as well.
         --
         --  * We should favor units whose caches were used recently, even if
         --    their lookup ratios are lower than that of another unit.

         Result : constant Boolean := Usefulness_Score < Entry_Ratio;
         --  We want to collect this unit if the usefulness of its cache
         --  entries is lower than the proportion of total memory space needed
         --  to store them.
      begin
         if Cache_Invalidation_Trace.Is_Active then
            if Result then
               Cache_Invalidation_Trace.Trace
                 ("Collecting " & Trace_Image (Unit));
            else
               Cache_Invalidation_Trace.Trace
                 ("Leaving alone " & Trace_Image (Unit));
            end if;

            Cache_Invalidation_Trace.Increase_Indent;
            Cache_Invalidation_Trace.Trace
              ("Cache entries:" & Unit_Stats.Entry_Count'Image);
            Cache_Invalidation_Trace.Trace
              ("Cache lookup count:" & Unit_Stats.Lookup_Count'Image);
            Cache_Invalidation_Trace.Trace
              ("Cache hit count:" & Unit_Stats.Hit_Count'Image);
            Cache_Invalidation_Trace.Trace
              ("Ratio of cache hits:"
               & Float'Image (100.0 * Hit_Ratio));
            Cache_Invalidation_Trace.Trace
              ("Ratio of total cache lookups:"
               & Float'Image (100.0 * Lookup_Ratio));
            Cache_Invalidation_Trace.Trace
              ("Ratio of recent cache lookups:"
               & Float'Image (100.0 * Recent_Lookup_Ratio));
            Cache_Invalidation_Trace.Trace
              ("Cache usefulness:"
               & Float'Image (100.0 * Usefulness_Score));
            Cache_Invalidation_Trace.Trace
              ("Ratio of entries:"
               & Float'Image (100.0 * Entry_Ratio));
            Cache_Invalidation_Trace.Decrease_Indent;
         end if;
         return Result;
      end;
   end Should_Collect_Env_Caches;

end Libadalang.Implementation.Extensions;
