with Langkit_Support.Slocs;
use type Langkit_Support.Slocs.Line_Number;
use type Langkit_Support.Slocs.Column_Number;

package body Highlighter is

   package Slocs renames Langkit_Support.Slocs;

   use type LAL.Token_Type;
   use type LAL.Ada_Node_Kind_Type;
   use Libadalang.Lexer;

   Basic_Highlights : constant
     array (Libadalang.Lexer.Token_Kind) of Highlight_Type
     := (Ada_Termination                  => Text,
         Ada_Lexing_Failure               => Text,
         Ada_Identifier                   => Identifier,
         Ada_All .. Ada_Return
           | Ada_Elsif | Ada_Reverse
           | Ada_End .. Ada_Select
           | Ada_Exception
           | Ada_Separate | Ada_Exit | Ada_Others
           | Ada_For | Ada_Out | Ada_Function | Ada_At
           | Ada_Generic .. Ada_Body
           | Ada_Then .. Ada_In
           | Ada_Is .. Ada_Declare
           | Ada_Delay | Ada_Until | Ada_When | Ada_Loop | Ada_While
           | Ada_Renames | Ada_Do | Ada_Requeue
         => Keyword,

         Ada_Subtype | Ada_Record => Keyword_Type,

         Ada_Abstract | Ada_Access | Ada_Aliased | Ada_Array | Ada_Constant
           | Ada_Delta | Ada_Digits | Ada_Limited | Ada_Of | Ada_Private
           | Ada_Range | Ada_Tagged
         => Keyword_Special,

         Ada_Par_Close .. Ada_Dot         => Punctuation_Special,
         Ada_Diamond                      => Keyword,
         Ada_Abs | Ada_And | Ada_Mod | Ada_Not | Ada_Or | Ada_Rem | Ada_Some
           | Ada_Xor
           | Ada_Lte .. Ada_Divide
         => Operator,

         Ada_Tick                         => Punctuation,
         Ada_Pipe | Ada_Assign            => Punctuation_Special,
         Ada_Label_Start .. Ada_Label_End => Label_Name,
         Ada_String                       => String_Literal,
         Ada_Char                         => Character_Literal,
         Ada_With                         => Keyword,
         Ada_Decimal                      => Integer_Literal,
         Ada_Integer                      => Integer_Literal,
         Ada_Comment                      => Comment,
         Ada_Prep_Line                    => Preprocessor_Directive);
   --  For each token kind, associate a default highlighting type

   procedure Highlight_Name
     (Name       : access LAL.Name_Type'Class;
      HL         : Highlight_Type;
      Highlights : in out Highlights_Holder);
   --  Assign the HL highlighting type to the main identifiers in Name

   procedure Highlight_Block_Name
     (Name       : access LAL.Name_Type'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Block_Name highlighting type to the main identifiers in Name

   procedure Highlight_Attribute_Ref
     (Id         : access LAL.Identifier_Type'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Attribute_Name highlighting type to the "'Name" tokens in Id

   procedure Highlight_Type_Expr
     (Expr       : access LAL.Type_Expr_Type'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Type_Name highlighting type to the main type name in Expr

   ---------
   -- Get --
   ---------

   function Get
     (Highlights : Highlights_Holder;
      Token      : LAL.Token_Data_Type) return Highlight_Type
   is
      Index : constant Token_Index := LAL.Index (Token);
   begin
      return (if LAL.Is_Trivia (Token)
              then Highlights.Trivia_Highlights (Index)
              else Highlights.Token_Highlights (Index));
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Highlights : in out Highlights_Holder;
      Token      : LAL.Token_Data_Type;
      HL         : Highlight_Type)
   is
      Index : constant Token_Index := LAL.Index (Token);
   begin
      if LAL.Is_Trivia (Token) then
         Highlights.Trivia_Highlights (Index) := HL;
      else
         Highlights.Token_Highlights (Index) := HL;
      end if;
   end Set;

   ---------------
   -- Set_Range --
   ---------------

   procedure Set_Range
     (Highlights  : in out Highlights_Holder;
      First, Last : LAL.Token_Type;
      HL          : Highlight_Type)
   is
      Cur : LAL.Token_Type := First;
   begin
      while Cur /= LAL.No_Token loop
         Set (Highlights, LAL.Data (Cur), HL);
         exit when Cur = Last;
         Cur := LAL.Next (Cur);
      end loop;
   end Set_Range;

   --------------------
   -- Highlight_Name --
   --------------------

   procedure Highlight_Name
     (Name       : access LAL.Name_Type'Class;
      HL         : Highlight_Type;
      Highlights : in out Highlights_Holder) is
   begin
      if Name = null then
         return;
      end if;

      case Name.Kind is
         when LAL.Ada_Identifier | LAL.Ada_String_Literal =>

            --  Highlight the only token that this node has

            declare
               Tok : constant LAL.Token_Type :=
                 LAL.Single_Tok_Node (Name).F_Tok;
            begin
               Set (Highlights, LAL.Data (Tok), HL);
            end;

         when LAL.Ada_Dotted_Name =>

            --  Highlight both the prefix, the suffix and the dot token

            declare
               Dotted_Name : constant LAL.Dotted_Name :=
                  LAL.Dotted_Name (Name);
               Dot_Token   : constant LAL.Token_Type :=
                  LAL.Next (Dotted_Name.F_Prefix.Token_End);
            begin
               Highlight_Name (Dotted_Name.F_Prefix, HL, Highlights);
               Set (Highlights, LAL.Data (Dot_Token), HL);
               Highlight_Name (Dotted_Name.F_Suffix, HL, Highlights);
            end;

         when LAL.Ada_Call_Expr =>

            --  Just highlight the name of the called entity

            Highlight_Name (LAL.Call_Expr (Name).F_Name, HL, Highlights);

         when others =>
            return;
      end case;
   end Highlight_Name;

   --------------------------
   -- Highlight_Block_Name --
   --------------------------

   procedure Highlight_Block_Name
     (Name       : access LAL.Name_Type'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Name = null then
         return;
      end if;
      Highlight_Name (Name, Block_Name, Highlights);
   end Highlight_Block_Name;

   -----------------------------
   -- Highlight_Attribute_Ref --
   -----------------------------

   procedure Highlight_Attribute_Ref
     (Id         : access LAL.Identifier_Type'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Id = null then
         return;
      end if;

      --  Set style for both the attribute name and the leading 'tick' token

      Set (Highlights, LAL.Data (LAL.Previous (Id.F_Tok)), Attribute_Name);
      Set (Highlights, LAL.Data (Id.F_Tok), Attribute_Name);
   end Highlight_Attribute_Ref;

   -------------------------
   -- Highlight_Type_Expr --
   -------------------------

   procedure Highlight_Type_Expr
     (Expr       : access LAL.Type_Expr_Type'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Expr = null then
         return;
      end if;

      case LAL.Kind (Expr) is
         when LAL.Ada_Anonymous_Type =>
            null;

         when LAL.Ada_Subtype_Indication =>
            Highlight_Name
              (LAL.Subtype_Indication (Expr).F_Name, Type_Name, Highlights);

         when others =>

            --  There should be no other kind for a Type_Expr node

            raise Program_Error;
      end case;
   end Highlight_Type_Expr;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (Unit       : LAL.Analysis_Unit;
      Highlights : in out Highlights_Holder)
   is
      function Syntax_Highlight
        (Node : access LAL.Ada_Node_Type'Class) return LAL.Visit_Status;
      --  Function to be called on all AST nodes in Unit. This is the
      --  *syntax* highlighting algorithm entry point.

      ----------------------
      -- Syntax_Highlight --
      ----------------------

      function Syntax_Highlight
        (Node : access LAL.Ada_Node_Type'Class) return LAL.Visit_Status
      is
      begin
         case Node.Kind is

            -----------------
            -- Block names --
            -----------------

            --  For constructs that define a scope with a name, highlight this
            --  name as a Block_Name. For instance: the name of a package, of a
            --  subprogram, of a type declaration, ...

            when LAL.Ada_Base_Package_Decl =>
               declare
                  Pkg_Decl : constant LAL.Base_Package_Decl :=
                    LAL.Base_Package_Decl (Node);
               begin
                  Highlight_Block_Name (Pkg_Decl.F_Package_Name, Highlights);
                  Highlight_Block_Name (Pkg_Decl.F_End_Id, Highlights);
               end;
            when LAL.Ada_Package_Body =>
               declare
                  Pkg_Body : constant LAL.Package_Body :=
                    LAL.Package_Body (Node);
               begin
                  Highlight_Block_Name (Pkg_Body.F_Package_Name, Highlights);
                  Highlight_Block_Name (Pkg_Body.F_End_Id, Highlights);
               end;

            when LAL.Ada_Package_Renaming_Decl =>
               Highlight_Block_Name
                 (LAL.Package_Renaming_Decl (Node).F_Name, Highlights);

            when LAL.Ada_Generic_Package_Instantiation =>
               Highlight_Block_Name
                 (LAL.Generic_Package_Instantiation (Node).F_Name, Highlights);
            when LAL.Ada_Generic_Subp_Instantiation =>
               Highlight_Block_Name
                 (LAL.Generic_Subp_Instantiation (Node).F_Subp_Name,
                  Highlights);

            when LAL.Ada_Subp_Spec =>
               declare
                  Subp_Spec : constant LAL.Subp_Spec := LAL.Subp_Spec (Node);
                  Params    : constant LAL.Param_Spec_Array_Access :=
                    Subp_Spec.P_Node_Params;
               begin
                  Highlight_Block_Name (Subp_Spec.F_Subp_Name, Highlights);
                  Highlight_Type_Expr (Subp_Spec.F_Subp_Returns, Highlights);
                  for Param of Params.Items loop
                     Highlight_Type_Expr (Param.F_Type_Expr, Highlights);
                  end loop;
               end;
            when LAL.Ada_Subp_Body =>
               Highlight_Block_Name
                 (LAL.Subp_Body (Node).F_End_Id, Highlights);

            when LAL.Ada_Type_Decl =>
               Set (Highlights, LAL.Data (Node.Token_Start), Keyword_Type);
               Highlight_Block_Name
                 (LAL.Type_Decl (Node).F_Type_Id, Highlights);

            when LAL.Ada_Subtype_Decl =>
               Highlight_Block_Name
                 (LAL.Subtype_Decl (Node).F_Type_Id, Highlights);

            when LAL.Ada_Named_Stmt_Decl =>
               Highlight_Block_Name
                 (LAL.Named_Stmt_Decl (Node).F_Name, Highlights);
            when LAL.Ada_Base_Loop_Stmt =>
               Highlight_Block_Name
                 (LAL.Base_Loop_Stmt (Node).F_End_Id, Highlights);
            when LAL.Ada_Decl_Block =>
               Highlight_Block_Name
                 (LAL.Decl_Block (Node).F_End_Id, Highlights);

            when LAL.Ada_Begin_Block =>
               Highlight_Block_Name
                 (LAL.Begin_Block (Node).F_End_Id, Highlights);

            --  TODO??? Still lots of nodes to handle! Protected types, tasks,
            --  etc.

            ----------------
            -- Type names --
            ----------------

            --  When it's "useful" (for readers), highlight type names as
            --  Type_Name.

            when LAL.Ada_Type_Access_Def =>
               Highlight_Name
                 (LAL.Type_Access_Def (Node).F_Subtype_Indication.F_Name,
                  Type_Name, Highlights);

            when LAL.Ada_Object_Decl =>
               Highlight_Type_Expr
                 (LAL.Object_Decl (Node).F_Type_Expr, Highlights);

            when LAL.Ada_Use_Type_Clause =>
               declare
                  Types : constant LAL.Name_List :=
                    LAL.Use_Type_Clause (Node).F_Types;
               begin
                  for I in 1 .. Types.Child_Count loop
                     Highlight_Name
                       (LAL.Name (Types.Child (I)), Type_Name, Highlights);
                  end loop;
               end;

            when LAL.Ada_Discriminant_Spec =>
               Highlight_Type_Expr
                 (LAL.Discriminant_Spec (Node).F_Type_Expr, Highlights);
            when LAL.Ada_Component_Def =>
               Highlight_Type_Expr
                 (LAL.Component_Def (Node).F_Type_Expr, Highlights);

            ----------
            -- Misc --
            ----------

            when LAL.Ada_Attribute_Ref =>
               Highlight_Attribute_Ref
                 (LAL.Attribute_Ref (Node).F_Attribute, Highlights);

            when LAL.Ada_Label_Decl =>
               Highlight_Name
                 (LAL.Label_Decl (Node).F_Name, Label_Name, Highlights);

            when LAL.Ada_Record_Def =>
               Set (Highlights,
                    LAL.Data (LAL.Previous (Node.Token_End)), Keyword_Type);

            when LAL.Ada_Null_Record_Def =>
               Set_Range
                 (Highlights, Node.Token_Start, Node.Token_End, Keyword_Type);

            when LAL.Ada_Aspect_Spec =>
               Set (Highlights, LAL.Data (Node.Token_Start), Keyword_Special);

            when LAL.Ada_Quantified_Expr =>
               Set (Highlights, LAL.Data (LAL.Next (Node.Token_Start)),
                    Operator);

            when LAL.Ada_Op =>
               if Node.Kind /= LAL.Ada_Op_Double_Dot then
                  Set_Range
                    (Highlights, Node.Token_Start, Node.Token_End, Operator);
               end if;

            when others =>
               null;
         end case;

         return LAL.Into;
      end Syntax_Highlight;

      Token : LAL.Token_Type := LAL.First_Token (Unit);

   --  Start of processing for Highlight

   begin
      --  Lexical pass: first, assign a "default" highlighting to all tokens
      --  just based on their kind. Also build the list of subhighlights.

      while Token /= LAL.No_Token loop
         declare
            TD : constant LAL.Token_Data_Type := LAL.Data (Token);
            HL : constant Highlight_Type := Basic_Highlights (LAL.Kind (TD));
         begin
            Set (Highlights, TD, HL);
         end;
         Token := LAL.Next (Token);
      end loop;

      --  Syntactic pass: update highlighting for tokens depending on how they
      --  are used in the syntax tree (subprogram names, type names, etc).

      LAL.Traverse (LAL.Root (Unit), Syntax_Highlight'Access);
   end Highlight;

   ----------------
   -- Put_Tokens --
   ----------------

   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlights_Holder)
   is
      Token     : LAL.Token_Type := LAL.First_Token (Unit);
      Last_Sloc : Slocs.Source_Location := (1, 1);
   begin
      while Token /= LAL.No_Token loop
         declare
            TD         : constant LAL.Token_Data_Type := LAL.Data (Token);
            HL         : constant Highlight_Type := Get (Highlights, TD);
            Sloc_Range : constant Slocs.Source_Location_Range :=
              LAL.Sloc_Range (TD);
            Text       : constant Langkit_Support.Text.Text_Type :=
              LAL.Text (Token);
         begin
            while Last_Sloc.Line < Sloc_Range.Start_Line loop
               New_Line;
               Last_Sloc.Line := Last_Sloc.Line + 1;
               Last_Sloc.Column := 1;
            end loop;
            if Sloc_Range.Start_Column > Last_Sloc.Column then
               Indent (Integer (Sloc_Range.Start_Column - Last_Sloc.Column));
            end if;

            Put_Token (Text, HL);
            Last_Sloc := Slocs.End_Sloc (Sloc_Range);
         end;
         Token := LAL.Next (Token);
      end loop;
   end Put_Tokens;

end Highlighter;
