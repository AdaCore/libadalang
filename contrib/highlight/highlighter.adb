package body Highlighter is

   use type LAL.Token_Type;
   use Libadalang.Lexer;

   Basic_Highlights : constant
     array (Libadalang.Lexer.Token_Kind) of Highlight_Type
     := (Ada_Termination                  => Text,
         Ada_Lexing_Failure               => Text,
         Ada_Identifier                   => Identifier,
         Ada_All .. Ada_Xor               => Keyword,
         Ada_Par_Close .. Ada_Diamond     => Punctuation,
         Ada_Lte .. Ada_Divide            => Operator,
         Ada_Tick                         => Punctuation,
         Ada_Pipe                         => Punctuation,
         Ada_Assign                       => Operator,
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
         when LAL.Ada_Dotted_Name =>

            --  Highlight both the prefix, the suffix and the dot token

            declare
               use all type LAL.Ada_Node_Type, LAL.Ada_Node;
               Dotted_Name : constant LAL.Dotted_Name :=
                 LAL.Dotted_Name (Name);
               Dot_Token   : constant LAL.Token_Type :=
                 (if LAL.Is_Null (Dotted_Name.F_Prefix)
                  then LAL.No_Token
                  else LAL.Next (Dotted_Name.F_Prefix.Token_End));
            begin
               Highlight_Name (Dotted_Name.F_Prefix, HL, Highlights);
               if Dot_Token /= LAL.No_Token then
                  Set (Highlights, LAL.Data (Dot_Token), HL);
               end if;
               Highlight_Name (Dotted_Name.F_Suffix, HL, Highlights);
            end;

         when LAL.Ada_Call_Expr =>

            --  Just highlight the name of the called entity

            Highlight_Name (LAL.Call_Expr (Name).F_Name, HL, Highlights);

         when LAL.Ada_Identifier | LAL.Ada_String_Literal =>
            declare
               Tok : constant LAL.Token_Type :=
                 LAL.Single_Tok_Node (Name).F_Tok;
            begin
               Set (Highlights, LAL.Data (Tok), HL);
            end;

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

            when LAL.Ada_Base_Package_Decl | LAL.Ada_Package_Decl =>
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
                 (LAL.Generic_Subp_Instantiation (Node).F_Name, Highlights);

            when LAL.Ada_Subp_Spec =>
               declare
                  Subp_Spec : constant LAL.Subp_Spec := LAL.Subp_Spec (Node);
                  Params    : constant LAL.Param_Spec_List :=
                    Subp_Spec.F_Subp_Params;
               begin
                  Highlight_Block_Name (Subp_Spec.F_Subp_Name, Highlights);
                  Highlight_Type_Expr (Subp_Spec.F_Subp_Returns, Highlights);
                  for I in 1 .. Params.Child_Count loop
                     Highlight_Type_Expr
                       (LAL.Param_Spec (Params.Child (I)).F_Type_Expr,
                        Highlights);
                  end loop;
               end;
            when LAL.Ada_Subp_Body =>
               Highlight_Block_Name
                 (LAL.Subp_Body (Node).F_End_Id, Highlights);

            when LAL.Ada_Type_Decl =>
               Highlight_Block_Name
                 (LAL.Type_Decl (Node).F_Type_Id, Highlights);

            when LAL.Ada_Subtype_Decl =>
               Highlight_Block_Name
                 (LAL.Subtype_Decl (Node).F_Type_Id, Highlights);

            when LAL.Ada_Named_Stmt_Decl =>
               Highlight_Block_Name
                 (LAL.Named_Stmt_Decl (Node).F_Name, Highlights);
            when LAL.Ada_Loop_Stmt =>
               Highlight_Block_Name
                 (LAL.Loop_Stmt (Node).F_End_Id, Highlights);
            when LAL.Ada_Block_Stmt =>
               Highlight_Block_Name
                 (LAL.Block_Stmt (Node).F_End_Id, Highlights);

            --  ??? Still lots of nodes to handle! Protected types, tasks, etc.

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

end Highlighter;
