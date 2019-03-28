with Ada.Strings.Unbounded;

with GNATCOLL.Strings;
with Langkit_Support.Slocs;
with Langkit_Support.Text;
with Libadalang.Common;

--  with GNATCOLL.Iconv;

package body HTML is

   package LALCO renames Libadalang.Common;

   Hex_Digits : constant
     array (Colors.Color_Level range 0 .. 15) of Character :=
     "0123456789abcdef";

   ------------
   -- Escape --
   ------------

   function Escape (S : String) return String is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for C of S loop
         case C is
            when '"'    => Append (Result, "&quot;");
            when '&'    => Append (Result, "&amp;");
            when '<'    => Append (Result, "&lt;");
            when '>'    => Append (Result, "&gt;");
            when others => Append (Result, C);
         end case;
      end loop;
      return To_String (Result);
   end Escape;

   -------------------
   -- Color_To_HTML --
   -------------------

   function Color_To_HTML (Color : Colors.Color_Type) return HTML_Color is
      use type Colors.Color_Level;
      Result : HTML_Color;
   begin
      Result (1) := Hex_Digits (Color.Red / 16);
      Result (2) := Hex_Digits (Color.Red mod 16);
      Result (3) := Hex_Digits (Color.Green / 16);
      Result (4) := Hex_Digits (Color.Green mod 16);
      Result (5) := Hex_Digits (Color.Blue / 16);
      Result (6) := Hex_Digits (Color.Blue mod 16);
      return Result;
   end Color_To_HTML;

   -------------------
   -- Put_CSS_Rules --
   -------------------

   procedure Put_CSS_Rules (S : Colors.Style_Type) is
   begin
      Put ("pre.code_highlight { background-color: #"
           & Color_To_HTML (S.Background_Color) & "; }" & ASCII.LF);
      for HL in Highlighter.Highlight_Type'Range loop
         declare
            Style : Colors.Token_Style renames S.Tok_Styles (HL);
         begin
            Put ("pre.code_highlight span."
                 & Highlighter.Highlight_Name (HL) & " {");
            Put (" color: #" & Color_To_HTML (Style.Color) & ";");
            if Style.Bold then
               Put (" font-weight: bold;");
            end if;
            Put (" }" & ASCII.LF);
         end;
      end loop;
   end Put_CSS_Rules;

   ----------------
   -- Put_Tokens --
   ----------------

   procedure Put_Tokens
     (Unit       : LAL.Analysis_Unit;
      Highlights : Highlighter.Highlights_Holder;
      Charset    : String;
      With_Xrefs : Boolean := False)
   is
      pragma Unreferenced (Charset);
      --  TODO: use Charset to properly encode token text (see Escape below)

      function Line_Anchor (Line : Natural) return String;
      --  Name of the anchor for the given line

      function Escape (T : Langkit_Support.Text.Text_Type) return String
      is (Escape (Langkit_Support.Text.Image (T)));

      procedure Put_Token
        (Token : LALCO.Token_Reference;
         Data  : LALCO.Token_Data_Type;
         HL   : Highlighter.Highlight_Type);
      procedure New_Line;
      procedure Add_Whitespace (C : Character);
      --  Generic parameters for Put_Tokens below

      Xrefs : array (1 .. LAL.Token_Count (Unit)) of LAL.Basic_Decl;
      --  For each token, No_Basic_Decl for no cross-reference, or the
      --  declaration to which the token should link.

      function Traverse (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;
      --  Callback for AST traversal. Return "Into" in all cases. When visiting
      --  a string literal or an identifier, perform name resolution on it and
      --  record the resulting declaration in the Xrefs array.

      Current_Line : Positive := 1;
      --  Line number for the tokens to be emitted

      Empty_Line : Boolean := True;
      --  Whether the current line is empty

      -----------------
      -- Line_Anchor --
      -----------------

      function Line_Anchor (Line : Natural) return String is
      begin
         return 'L' & GNATCOLL.Strings.To_XString
           (Natural'Image (Line)).Trim.To_String;
      end Line_Anchor;

      ---------------
      -- Put_Token --
      ---------------

      procedure Put_Token
        (Token : LALCO.Token_Reference;
         Data  : LALCO.Token_Data_Type;
         HL    : Highlighter.Highlight_Type)
      is
         Text : constant Langkit_Support.Text.Text_Type := LALCO.Text (Token);

         Decl : constant LAL.Basic_Decl :=
           (if LALCO.Is_Trivia (Data)
            then LAL.No_Basic_Decl
            else Xrefs (Natural (LALCO.Index (Token))));
         --  The declaration that xrefs associated to this token, if any
      begin

         --  Emit decoration for xref information, if any
         if not Decl.Is_Null then
            declare
               Unit     : constant LAL.Analysis_Unit := Decl.Unit;
               Href     : constant String := URL (Unit);
               Line_Raw : constant String :=
                 Langkit_Support.Slocs.Line_Number'Image
                   (Decl.Sloc_Range.Start_Line);
               Line     : constant String :=
                 GNATCOLL.Strings.To_XString (Line_Raw).Trim.To_String;
            begin
               --  If the declaration for this token is in the scope of the set
               --  of HTML documents we generate, create a hyperlink. In all
               --  cases, create a label for the token.
               Put ("<a");
               if Href /= "" then
                  Put (" href=""" & Escape (Href) & "#L" & Line & """");
               end if;
               Put (" title=""" & Escape (LAL.Get_Filename (Unit))
                    & ", line " & Line & """");
               Put (">");
            end;
         end if;

         --  Emit the highlighted token/trivia itself
         Put ("<span class=""" & Highlighter.Highlight_Name (HL) & """>");
         Put (Escape (Text));
         Put ("</span>");

         if not Decl.Is_Null then
            Put ("</a>");
         end if;

         Empty_Line := False;
      end Put_Token;

      --------------
      -- New_Line --
      --------------

      procedure New_Line is
      begin
         Current_Line := Current_Line + 1;
         Put ("</span>");
         if Empty_Line then
            Put ((1 => ASCII.LF));
         end if;
         Put ("<span class=""line"" id="""
              & Line_Anchor (Current_Line) & """>");

         Empty_Line := True;
      end New_Line;

      ------------
      -- Indent --
      ------------

      procedure Add_Whitespace (C : Character) is
      begin
         Empty_Line := False;
         Put ((1 => C));
      end Add_Whitespace;

      --------------
      -- Traverse --
      --------------

      function Traverse (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
      is
      begin
         --  We only annnotate leaf nodes for xrefs
         if Node.Kind not in LALCO.Ada_String_Literal | LALCO.Ada_Identifier
         then
            return LALCO.Into;
         end if;

         --  Try to perform name resolution on this single-token node. Discard
         --  errors.
         declare
            Token : constant LALCO.Token_Reference :=
               Node.As_Single_Tok_Node.Token_Start;
            Index : constant Natural := Natural (LALCO.Index (Token));
            Decl  : LAL.Basic_Decl renames Xrefs (Index);
         begin
            Decl := Node.As_Name.P_Referenced_Decl;
         exception
            when LALCO.Property_Error =>
               Decl := LAL.No_Basic_Decl;
         end;
         return LALCO.Into;
      end Traverse;

      procedure Put_Tokens is new Highlighter.Put_Tokens;
   begin

      --  Create the Xrefs array if asked to
      if With_Xrefs then
         for Xref of Xrefs loop
            Xref := LAL.No_Basic_Decl;
         end loop;
         LAL.Root (Unit).Traverse (Traverse'Access);
      end if;

      --  Then emit HTML tags for the highlighted source code
      Put ("<pre class=""code_highlight"">");
      Put ("<span class=""inline"" id=""" & Line_Anchor (1) & """>");
      Put_Tokens (Unit, Highlights);
      Put ("</span>");
      Put ("</pre>");
   end Put_Tokens;

end HTML;
