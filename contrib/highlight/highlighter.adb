with Langkit_Support.Text;

package body Highlighter is

   package Chars renames Langkit_Support.Text.Chars;

   use type LALCO.Token_Reference;
   use type LALCO.Ada_Node_Kind_Type;
   use Libadalang.Common;

   Basic_Highlights : constant
     array (LALCO.Token_Kind) of Highlight_Type
     := (Ada_Char                        => Character_Literal,
         Ada_Comment                     => Comment,
         Ada_Decimal | Ada_Integer       => Integer_Literal,
         Ada_Identifier                  => Identifier,
         Ada_Label_End | Ada_Label_Start => Label_Name,
         Ada_Prep_Line                   => Preprocessor_Directive,
         Ada_Subtype | Ada_Record        => Keyword_Type,
         Ada_Tick                        => Punctuation,

         Ada_Lexing_Failure
           | Ada_Termination
           | Ada_Whitespace
         => Text,

         Ada_Format_String_End
           | Ada_Format_String_Mid
           | Ada_Format_String_Start
           | Ada_Format_String_String
           | Ada_String
         => String_Literal,

         Ada_Abort
           | Ada_Accept
           | Ada_All
           | Ada_At
           | Ada_Begin
           | Ada_Body
           | Ada_Case
           | Ada_Declare
           | Ada_Delay
           | Ada_Diamond
           | Ada_Do
           | Ada_Else
           | Ada_Elsif
           | Ada_End
           | Ada_Entry
           | Ada_Exception
           | Ada_Exit
           | Ada_For
           | Ada_Function
           | Ada_Generic
           | Ada_Goto
           | Ada_If
           | Ada_In
           | Ada_Is
           | Ada_Loop
           | Ada_New
           | Ada_Null
           | Ada_Others
           | Ada_Out
           | Ada_Package
           | Ada_Pragma
           | Ada_Procedure
           | Ada_Raise
           | Ada_Renames
           | Ada_Return
           | Ada_Reverse
           | Ada_Select
           | Ada_Separate
           | Ada_Task
           | Ada_Terminate
           | Ada_Then
           | Ada_Type
           | Ada_Use
           | Ada_When
           | Ada_While
           | Ada_With
         => Keyword,

         Ada_Access
           | Ada_Array
           | Ada_Constant
           | Ada_Delta
           | Ada_Digits
           | Ada_Limited
           | Ada_Of
           | Ada_Private
           | Ada_Range
         => Keyword_Special,

         Ada_Assign
           | Ada_Brack_Close
           | Ada_Brack_Open
           | Ada_Colon
           | Ada_Comma
           | Ada_Dot
           | Ada_Doubledot
           | Ada_Par_Close
           | Ada_Par_Open
           | Ada_Pipe
           | Ada_Semicolon
         => Punctuation_Special,

         Ada_Abs
           | Ada_Amp
           | Ada_And
           | Ada_Arrow
           | Ada_Divide
           | Ada_Equal
           | Ada_Gt
           | Ada_Gte
           | Ada_Lt
           | Ada_Lte
           | Ada_Minus
           | Ada_Mod
           | Ada_Mult
           | Ada_Not
           | Ada_Notequal
           | Ada_Or
           | Ada_Plus
           | Ada_Power
           | Ada_Rem
           | Ada_Target
           | Ada_Xor
         => Operator);
   --  For each token kind, associate a default highlighting type

   procedure Highlight_Name
     (Name       : LAL.Name'Class;
      HL         : Highlight_Type;
      Highlights : in out Highlights_Holder);
   --  Assign the HL highlighting type to the main identifiers in Name

   procedure Highlight_Block_Name
     (Name       : LAL.Name'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Block_Name highlighting type to the main identifiers in Name

   procedure Highlight_Attribute_Ref
     (Id         : LAL.Identifier'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Attribute_Name highlighting type to the "'Name" tokens in Id

   procedure Highlight_Type_Expr
     (Expr       : LAL.Type_Expr'Class;
      Highlights : in out Highlights_Holder);
   --  Assign the Type_Name highlighting type to the main type name in Expr

   ---------
   -- Get --
   ---------

   function Get
     (Highlights : Highlights_Holder;
      Token      : LALCO.Token_Data_Type) return Highlight_Type
   is
      Index : constant Token_Index := LALCO.Index (Token);
   begin
      return (if LALCO.Is_Trivia (Token)
              then Highlights.Trivia_Highlights (Index)
              else Highlights.Token_Highlights (Index));
   end Get;

   ---------
   -- Set --
   ---------

   procedure Set
     (Highlights : in out Highlights_Holder;
      Token      : LALCO.Token_Data_Type;
      HL         : Highlight_Type)
   is
      Index : constant Token_Index := LALCO.Index (Token);
   begin
      if LALCO.Is_Trivia (Token) then
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
      First, Last : LALCO.Token_Reference;
      HL          : Highlight_Type)
   is
      Cur : LALCO.Token_Reference := First;
   begin
      while Cur /= LALCO.No_Token loop
         Set (Highlights, LALCO.Data (Cur), HL);
         exit when Cur = Last;
         Cur := LALCO.Next (Cur, Exclude_Trivia => True);
      end loop;
   end Set_Range;

   --------------------
   -- Highlight_Name --
   --------------------

   procedure Highlight_Name
     (Name       : LAL.Name'Class;
      HL         : Highlight_Type;
      Highlights : in out Highlights_Holder) is
   begin
      if Name.Is_Null then
         return;
      end if;

      case Name.Kind is
         when LALCO.Ada_Identifier | LALCO.Ada_String_Literal =>

            --  Highlight the only token that this node has

            declare
               Tok : constant LALCO.Token_Reference :=
                  Name.As_Single_Tok_Node.Token_Start;
            begin
               Set (Highlights, LALCO.Data (Tok), HL);
            end;

         when LALCO.Ada_Dotted_Name =>

            --  Highlight both the prefix, the suffix and the dot token

            declare
               Dotted_Name : constant LAL.Dotted_Name := Name.As_Dotted_Name;
               Dot_Token   : constant LALCO.Token_Reference :=
                  LALCO.Next (Dotted_Name.F_Prefix.Token_End,
                            Exclude_Trivia => True);
            begin
               Highlight_Name (Dotted_Name.F_Prefix, HL, Highlights);
               Set (Highlights, LALCO.Data (Dot_Token), HL);
               Highlight_Name (Dotted_Name.F_Suffix, HL, Highlights);
            end;

         when LALCO.Ada_Call_Expr =>

            --  Just highlight the name of the called entity

            Highlight_Name (Name.As_Call_Expr.F_Name, HL, Highlights);

         when LALCO.Ada_Defining_Name =>

            --  Highlight inner name
            Highlight_Name (Name.As_Defining_Name.F_Name, HL, Highlights);

         when others =>
            return;
      end case;
   end Highlight_Name;

   --------------------------
   -- Highlight_Block_Name --
   --------------------------

   procedure Highlight_Block_Name
     (Name       : LAL.Name'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Name.Is_Null then
         return;
      end if;
      Highlight_Name (Name, Block_Name, Highlights);
   end Highlight_Block_Name;

   -----------------------------
   -- Highlight_Attribute_Ref --
   -----------------------------

   procedure Highlight_Attribute_Ref
     (Id         : LAL.Identifier'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Id.Is_Null then
         return;
      end if;

      --  Set style for both the attribute name and the leading 'tick' token

      Set (Highlights,
           LALCO.Data (LALCO.Previous (Id.Token_Start,
                                       Exclude_Trivia => True)),
           Attribute_Name);
      Set (Highlights, LALCO.Data (Id.Token_Start), Attribute_Name);
   end Highlight_Attribute_Ref;

   -------------------------
   -- Highlight_Type_Expr --
   -------------------------

   procedure Highlight_Type_Expr
     (Expr       : LAL.Type_Expr'Class;
      Highlights : in out Highlights_Holder) is
   begin
      if Expr.Is_Null then
         return;
      end if;

      case LAL.Kind (Expr) is
         when LALCO.Ada_Anonymous_Type =>
            null;

         when LALCO.Ada_Subtype_Indication =>
            Highlight_Name
              (Expr.As_Subtype_Indication.F_Name, Type_Name, Highlights);

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
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
      --  Function to be called on all AST nodes in Unit. This is the
      --  *syntax* highlighting algorithm entry point.

      ----------------------
      -- Syntax_Highlight --
      ----------------------

      function Syntax_Highlight
        (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         case Node.Kind is

            -----------------
            -- Block names --
            -----------------

            --  For constructs that define a scope with a name, highlight this
            --  name as a Block_Name. For instance: the name of a package, of a
            --  subprogram, of a type declaration, ...

            when LALCO.Ada_Base_Package_Decl =>
               declare
                  Pkg_Decl : constant LAL.Base_Package_Decl :=
                     Node.As_Base_Package_Decl;
               begin
                  Highlight_Block_Name (Pkg_Decl.F_Package_Name, Highlights);
               end;
            when LALCO.Ada_Package_Body =>
               declare
                  Pkg_Body : constant LAL.Package_Body := Node.As_Package_Body;
               begin
                  Highlight_Block_Name (Pkg_Body.F_Package_Name, Highlights);
               end;

            when LALCO.Ada_Package_Renaming_Decl =>
               Highlight_Block_Name
                 (Node.As_Package_Renaming_Decl.F_Name, Highlights);

            when LALCO.Ada_Generic_Package_Instantiation =>
               Highlight_Block_Name
                 (Node.As_Generic_Package_Instantiation.F_Name, Highlights);
            when LALCO.Ada_Generic_Subp_Instantiation =>
               Highlight_Block_Name
                 (Node.As_Generic_Subp_Instantiation.F_Subp_Name,
                  Highlights);

            when LALCO.Ada_End_Name =>
               Highlight_Block_Name (Node.As_End_Name.F_Name, Highlights);

            when LALCO.Ada_Subp_Spec =>
               declare
                  Subp_Spec : constant LAL.Subp_Spec := Node.As_Subp_Spec;
                  Params    : constant LAL.Param_Spec_Array :=
                    Subp_Spec.P_Params;
               begin
                  Highlight_Block_Name (Subp_Spec.F_Subp_Name, Highlights);
                  Highlight_Type_Expr (Subp_Spec.F_Subp_Returns, Highlights);
                  for Param of Params loop
                     Highlight_Type_Expr (Param.F_Type_Expr, Highlights);
                  end loop;
               end;
            when LALCO.Ada_Type_Decl =>
               Set (Highlights, LALCO.Data (Node.Token_Start), Keyword_Type);
               Highlight_Block_Name
                 (Node.As_Type_Decl.F_Name, Highlights);

            when LALCO.Ada_Subtype_Decl =>
               Highlight_Block_Name
                 (Node.As_Subtype_Decl.F_Name, Highlights);

            when LALCO.Ada_Named_Stmt_Decl =>
               Highlight_Block_Name
                 (Node.As_Named_Stmt_Decl.F_Name, Highlights);

            --  TODO??? Still lots of nodes to handle! Protected types, tasks,
            --  etc.

            ----------------
            -- Type names --
            ----------------

            --  When it's "useful" (for readers), highlight type names as
            --  Type_Name.

            when LALCO.Ada_Type_Access_Def =>
               Highlight_Name
                 (Node.As_Type_Access_Def.F_Subtype_Indication.P_Type_Name,
                  Type_Name, Highlights);

            when LALCO.Ada_Object_Decl =>
               Highlight_Type_Expr
                 (Node.As_Object_Decl.F_Type_Expr, Highlights);

            when LALCO.Ada_Use_Type_Clause =>
               declare
                  Types : constant LAL.Name_List :=
                     Node.As_Use_Type_Clause.F_Types;
               begin
                  for I in 1 .. Types.Children_Count loop
                     Highlight_Name
                       (Types.Child (I).As_Name, Type_Name, Highlights);
                  end loop;
               end;

            when LALCO.Ada_Discriminant_Spec =>
               Highlight_Type_Expr
                 (Node.As_Discriminant_Spec.F_Type_Expr, Highlights);
            when LALCO.Ada_Component_Def =>
               Highlight_Type_Expr
                 (Node.As_Component_Def.F_Type_Expr, Highlights);

            ----------
            -- Misc --
            ----------

            when LALCO.Ada_Attribute_Ref =>
               Highlight_Attribute_Ref
                 (Node.As_Attribute_Ref.F_Attribute, Highlights);

            when LALCO.Ada_Label_Decl =>
               Highlight_Name
                 (Node.As_Label_Decl.F_Name, Label_Name, Highlights);

            when LALCO.Ada_Record_Def =>
               Set (Highlights,
                    LALCO.Data (LALCO.Previous (Node.Token_End,
                                                Exclude_Trivia => True)),
                    Keyword_Type);

            when LALCO.Ada_Null_Record_Def =>
               Set_Range
                 (Highlights, Node.Token_Start, Node.Token_End, Keyword_Type);

            when LALCO.Ada_Aspect_Spec =>
               Set (Highlights, LALCO.Data (Node.Token_Start),
                    Keyword_Special);

            when LALCO.Ada_Quantified_Expr =>
               Set (Highlights,
                    LALCO.Data (LALCO.Next (Node.Token_Start,
                                            Exclude_Trivia => True)),
                    Operator);

            when LALCO.Ada_Op =>
               if Node.Kind /= LALCO.Ada_Op_Double_Dot then
                  Set_Range
                    (Highlights, Node.Token_Start, Node.Token_End, Operator);
               end if;

            when others =>
               null;
         end case;

         return LALCO.Into;
      end Syntax_Highlight;

      Token : LALCO.Token_Reference := LAL.First_Token (Unit);

   --  Start of processing for Highlight

   begin
      --  Lexical pass: first, assign a "default" highlighting to all tokens
      --  just based on their kind. Also build the list of subhighlights.

      while Token /= LALCO.No_Token loop
         declare
            TD : constant LALCO.Token_Data_Type := LALCO.Data (Token);
            HL : constant Highlight_Type := Basic_Highlights (LALCO.Kind (TD));
         begin
            Set (Highlights, TD, HL);

            --  Some keywords don't have a token kind, because they're reserved
            --  words in LAL. Highlight those too.
            if LAL.Is_Keyword (Token, Libadalang.Common.Ada_2012) then
               Set (Highlights, TD, Keyword);
            end if;
         end;
         Token := LALCO.Next (Token);
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
      Token : LALCO.Token_Reference := LAL.First_Token (Unit);
   begin
      while Token /= LALCO.No_Token loop
         declare
            TD : constant LALCO.Token_Data_Type := LALCO.Data (Token);
            HL : constant Highlight_Type := Get (Highlights, TD);
         begin
            if LALCO.Kind (TD) = Ada_Whitespace then
               --  If this is a whitespace token, transmit its code layout
               --  change to the HTML document as appropriate.

               for C of Langkit_Support.Text.Text_Type'(LALCO.Text (Token))
               loop
                  case C is
                     when ' ' | Chars.HT | Chars.FF | Chars.CR =>
                        declare
                           ASCII_C : constant Character := Character'Val
                             (Langkit_Support.Text.Character_Type'Pos (C));
                        begin
                           Add_Whitespace (ASCII_C);
                        end;

                     when Chars.LF =>
                        New_Line;

                     when others =>
                        --  Whitespace tokens are not supposed to contain any
                        --  other character.
                        raise Program_Error;
                  end case;
               end loop;

            else
               Put_Token (Token, TD, HL);
            end if;
         end;
         Token := LALCO.Next (Token);
      end loop;
   end Put_Tokens;

end Highlighter;
