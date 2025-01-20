--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Containers.Vectors;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;

with System;

with GNAT.Strings; use GNAT.Strings;

with GNATCOLL.GMP.Integers;

with Langkit_Support.Slocs;    use Langkit_Support.Slocs;
with Langkit_Support.Symbols;  use Langkit_Support.Symbols;
with Langkit_Support.Text;     use Langkit_Support.Text;
with Langkit_Support.Token_Data_Handlers;
use Langkit_Support.Token_Data_Handlers;

with Libadalang.Common;               use Libadalang.Common;
with Libadalang.Lexer_Implementation; use Libadalang.Lexer_Implementation;

package body Libadalang.PP_Impl is

   Comment_Prefix : constant Text_Type := "--! ";
   --  Prefix to add to disabled lines when the line mode is ``Comment_Lines``

   type Eval_State is (Disabled, On, Off);
   --  See ``Branch_State.State`` below

   type Branch_State is record
      Start_Line : Line_Number;
      --  Line number for the ``#if`` directive

      State : Eval_State;
      --  Current state for this sequence of directives.
      --
      --  First, some terminology we use in this implementation:
      --
      --  * Preprocessor directives (``#if``/``#elsif``/``#else``/``#end if;``)
      --    make it possible to conditionally disable some parts of the
      --    source code to preprocess.
      --
      --  * A sequence is a list of corresponding directives, starting with an
      --    ``#if`` directive, and ending with an ``#end if;`` one.
      --
      --  * Directive in a sequence create a partition of blocks::
      --
      --       # if A then
      --          [block 1]
      --       # elsif B then
      --          [block 2]
      --       # else
      --          [block 3]
      --       # end if;
      --
      --    In a given sequence, at most one block can be "enabled": its code
      --    is part of the preprocessor output. Other blocks are said to be
      --    "disabled".
      --
      --  This ``State`` component is used to track how the evaluation of a
      --  sequence went so far:
      --
      --  ``Disabled`` is when this whole sequence of directives is part of
      --  disabled code (for instance when inside a
      --  ``#if False then ... # end if;`` block), or when the state was ``On``
      --  for a previous block in the same sequence. In other words,
      --  ``Disabled`` means "no other block in the current sequence will be
      --  enabled".
      --
      --  ``On`` is when the last directive (#if, #elsif or #else) started an
      --  enabled block. In this case, the next directive for this sequence
      --  will switch the state to ``Disabled``.
      --
      --  ``Off`` is no block in the current sequence has been active yet. In
      --  this case, another directive in the current sequence may still be
      --  active.

      Last_Was_Else : Boolean;
      --  Whether the last directive in this sequence was ``#else``.
   end record;
   --  Describes the state of evaluation for a given sequence of
   --  #if/#elsif/#else directives.

   package Branch_State_Vectors is new Ada.Containers.Vectors
     (Positive, Branch_State);
   --  Stack of branch evaluations, to properly handle nested directive
   --  sequences.

   type Enabled_Tokens_Array is array (Positive range <>) of Boolean;
   --  For each token in a source to preprocess, whether the token is enabled,
   --  i.e. whether it should be part of the output code.

   type Enabled_Tokens is access all Enabled_Tokens_Array;
   procedure Free is new Ada.Unchecked_Deallocation
     (Enabled_Tokens_Array, Enabled_Tokens);

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc        : Source_Location;
      Message     : Text_Type);
   --  Append a diagnostic for ``Sloc`` and ``Message`` to ``Diagnostics``.
   --  This is just a shortcut to create an "empty" sloc range from ``Sloc``.

   procedure Process_Tokens
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      TDH         : Token_Data_Handler;
      Tokens      : out Enabled_Tokens_Array;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Given a preprocessing configuration, go through all tokens in ``TDH``
   --  and initialize ``Tokens`` to create a map of all tokens to be
   --  enabled/disabled in the output source, appending to ``Diagnostics`` in
   --  case of errors.
   --
   --  ``Context`` is just used to parse preprocessing directives.

   procedure Emit_Tokens
     (Cfg         : File_Config;
      TDH         : Token_Data_Handler;
      Tokens      : Enabled_Tokens_Array;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Given a preprocessing configuration, allocate and fill a buffer for the
   --  preprocessed source using tokens from ``TDH`` and ``Tokens``  to decide
   --  whether to include these tokens. Append to ``Diagnostics`` in case of
   --  errors.

   procedure Process_Prep_Line
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      Text        : Text_Type;
      Sloc        : Source_Location;
      Prev_States : in out Branch_State_Vectors.Vector;
      State       : in out Branch_State;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Evaluate a preprocessor directive.
   --
   --  ``Cfg`` is the preprocessing configuration to use for the evaluation.
   --
   --  ``Context`` is just used to parse preprocessing directives.
   --
   --  ``Text`` is the source slice corresponding to the "content" of the
   --  preprocessor directive, i.e. the preprocessor directive token minus the
   --  leading '#'.
   --
   --  ``Sloc`` is the source location in the original source corresponding to
   --  the first codepoint in ``Text``.
   --
   --  ``State`` contains the evaluation state for the current sequence of
   --  directives while ``Prev_States`` contains the corresponding states for
   --  outer sequences (i.e. sequences that contain the current one).

   ------------
   -- Append --
   ------------

   procedure Append
     (Diagnostics : in out Diagnostics_Vectors.Vector;
      Sloc        : Source_Location;
      Message     : Text_Type) is
   begin
      Append (Diagnostics, Make_Range (Sloc, Sloc), Message);
   end Append;

   --------------------
   -- Process_Tokens --
   --------------------

   procedure Process_Tokens
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      TDH         : Token_Data_Handler;
      Tokens      : out Enabled_Tokens_Array;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Prev_States : Branch_State_Vectors.Vector;
      State       : Branch_State := (0, On, False);
      --  Currently processed branches. Updated each time we cross a
      --  preprocessor directive line (#if, #elsif, #else, #end if).

      Cur : Token_Or_Trivia_Index := First_Token_Or_Trivia (TDH);
      Tok : Stored_Token_Data;
      I   : Positive := Tokens'First;
   begin
      while Cur /= No_Token_Or_Trivia_Index loop
         Tok := Data (Cur, TDH);
         if To_Token_Kind (Tok.Kind) = Ada_Prep_Line then
            pragma Assert (TDH.Source_Buffer.all (Tok.Source_First) = '#');
            declare
               Buffer : Text_Type renames TDH.Source_Buffer.all
                 (Tok.Source_First + 1 .. Tok.Source_Last);
               Sloc   : Source_Location := Sloc_Start (TDH, Tok);
            begin
               --  Account for the missing '#' in the source buffer

               Sloc.Column := Sloc.Column + 1;

               Process_Prep_Line
                 (Cfg, Context, Buffer, Sloc, Prev_States, State, Diagnostics);
            end;
            Tokens (I) := False;
         else
            Tokens (I) := State.State = On;
         end if;
         I := I + 1;
         Cur := Next (Cur, TDH);
      end loop;

      if not Prev_States.Is_Empty then
         Append
           (Diagnostics,
            Source_Location'(State.Start_Line, 1),
            "missing corresponding ""#end if;""");
      end if;
   end Process_Tokens;

   -----------------
   -- Emit_Tokens --
   -----------------

   procedure Emit_Tokens
     (Cfg         : File_Config;
      TDH         : Token_Data_Handler;
      Tokens      : Enabled_Tokens_Array;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Buffer : Text_Type renames TDH.Source_Buffer.all;

      procedure Reserve (Size : Natural);
      --  Reallocate ``Contents`` if needed to ensure that is has room for
      --  ``Size`` extra bytes.

      procedure Append (Text : Text_Type);
      --  Append the given source excerpt to ``Contents``, converting
      --  each ``Character_Type`` item to ``Character`` (ranges are compatible
      --  by construction of the original source buffer).
      --
      --  ``Text`` *must* be a slice of ``Buffer``.
      --
      --  Note that this automatically removes/replaces CR bytes before
      --  adding bytes to ``Contents``.

      procedure Append (Text : String);
      --  Append the given source excerpt to ``Contents``

      function Next_Is_Disabled (Index : Positive) return Boolean
      is (Index < Tokens'Last and not Tokens (Index + 1));
      --  Return whether the token that follows the token at ``Index`` is
      --  disabled.

      -------------
      -- Reserve --
      -------------

      procedure Reserve (Size : Natural) is
         B : GNAT.Strings.String_Access;
      begin
         if Contents.Buffer.all'Last - Contents.Last < Size then

            --  There is not enough room in ``Contents.Buffer`` to host
            --  ``Size`` more bytes: allocate a bigger buffer, copy the
            --  existing content and free the previous buffer.

            B := new String (1 .. 2 * Contents.Buffer'Last);
            B (1 .. Contents.Last) := Contents.Buffer.all (1 .. Contents.Last);
            Free (Contents.Buffer);
            Contents.Buffer := B;
         end if;
      end Reserve;

      ------------
      -- Append --
      ------------

      procedure Append (Text : Text_Type) is
         I : Natural := Text'First;
      begin
         Reserve (Text'Length);
         while I <= Text'Last loop

            --  Replace each CRLF or CR byte sequence with a single LF byte.
            --
            --  Since ``Append`` can be called with "...[CR]" and then with
            --  "[LF]..." strings, we must skip CR bytes when they are followed
            --  with LF bytes even if that LF byte is out of the range of
            --  ``Text``. This is fine, as ``Text`` is a ``Buffer`` slice.

            if Text (I) /= Chars.CR then
               Contents.Last := Contents.Last + 1;
               Contents.Buffer.all (Contents.Last) :=
                 Character'Val (Character_Type'Pos (Text (I)));

            elsif I = Buffer'Last or else Buffer (I + 1) /= Chars.LF then
               Contents.Last := Contents.Last + 1;
               Contents.Buffer.all (Contents.Last) := ASCII.LF;
            end if;

            I := I + 1;
         end loop;
      end Append;

      ------------
      -- Append --
      ------------

      procedure Append (Text : String) is
      begin
         Reserve (Text'Length);
         Contents.Buffer (Contents.Last + 1 .. Contents.Last + Text'Length) :=
           Text;
         Contents.Last := Contents.Last + Text'Length;
      end Append;

      Cur : Token_Or_Trivia_Index := First_Token_Or_Trivia (TDH);
      --  Currently processed token in the original source: we process all of
      --  them in a sequence.

      Enabled : Boolean;
      --  Shortcut for ``Tokens (Index)`` for the ``Index`` corresponding to
      --  the current token.

      Tok : Stored_Token_Data;
      --  Data for the current token

      Kind : Token_Kind;
      --  Kind for the current token

      Previous_Enabled : Boolean := True;
      --  Whether the previous token was enabled, i.e. included as enabled code
      --  in the output buffer. This is used to determine when to strip
      --  disabled indentation, to emit comment prefixes, and so on.
   begin
      --  In the most common case, the preprocessed source buffer will be at
      --  least as large as the original buffer.

      Contents := (Buffer => new String (1 .. Buffer'Length),
                   Last   => 0);

      --  For disabled code, we append comment prefixes when processing line
      --  feed bytes, which can only occur in whitespace tokens. If the first
      --  token is disabled, there is no line feed before, so we will not have
      --  yet a chance to emit a comment prefix: do it now.

      if Tokens'Length > 0
         and then not Tokens (Tokens'First)
         and then Cfg.Line_Mode = Comment_Lines
      then
         Append (Comment_Prefix);
      end if;

      --  Go through all original tokens and emit the corresponding
      --  preprocessed tokens to ``Contents``.

      for Index in Tokens'Range loop
         Enabled := Tokens (Index);
         Tok := Data (Cur, TDH);
         Kind := To_Token_Kind (Tok.Kind);

         if Enabled then

            --  Forward enabled tokens as-is, with two exceptions

            if Kind = Ada_Identifier and then Buffer (Tok.Source_First) = '$'
            then
               --  Preprocessing symbols must be expanded to the corresponding
               --  value.

               declare
                  use Definition_Maps;
                  Sym : Text_Type renames
                    Buffer (Tok.Source_First + 1 ..  Tok.Source_Last);
                  Cur : constant Cursor := Cfg.Definitions.Find
                    (US.To_Unbounded_String (To_Lower (Image (Sym))));
               begin
                  if Has_Element (Cur) then
                     Append (US.To_String (As_String (Element (Cur))));
                  else
                     Append (Diagnostics,
                             Sloc_Range (TDH, Tok),
                             "unknown symbol """ & Sym & """");
                     Append (Sym);
                  end if;
               end;

            elsif Kind = Ada_Whitespace then

               --  Amend whitespaces depending on whether the tokens
               --  surrounding it are enabled.

               declare
                  First        : Natural := Tok.Source_First;
                  Last         : Positive := Tok.Source_Last;
                  Disable_Next : constant Boolean := Next_Is_Disabled (Index);
               begin
                  --  The only possible transition from a disabled token to an
                  --  enabled one is: from a directive (they are always
                  --  disabled) to the whitespace token that follows it (at
                  --  least one new line codepoint).
                  --
                  --  If disabled lines should be deleted from the output, do
                  --  not emit this extra line feed.

                  if not Previous_Enabled and then Cfg.Line_Mode = Delete_Lines
                  then
                     pragma Assert (Buffer (First) = Chars.LF);
                     First := First + 1;
                  end if;

                  --  The only possible transition from an enabled token to a
                  --  disabled one is: from a whitespace token (at least one
                  --  line line codepoint, followed by optional indentation) to
                  --  a directive (which are always disabled). We know at this
                  --  point that the current token is an enabled whitespace, so
                  --  if the next token is disabled, we know it is a directive.

                  if Disable_Next then

                     --  We need special handling for preprocessing directives.
                     --
                     --  Indented preprocessor directives are allowed, and the
                     --  indentation should go with the directive in the
                     --  output: in delete/blank lines modes, it should go
                     --  away, and in comment lines mode, it should occur after
                     --  the comment prefix.

                     while Last >= First and then Buffer (Last) /= Chars.LF
                     loop
                        Last := Last - 1;
                     end loop;
                  end if;

                  --  Append the rest of the whitespace token

                  Append (Buffer (First .. Last));

                  --  Append the comment line and then the directive
                  --  indentation if the line mode requires it.

                  if Disable_Next and then Cfg.Line_Mode = Comment_Lines then
                     Append (Comment_Prefix);
                     Append (Buffer (Last + 1 .. Tok.Source_Last));
                  end if;
               end;

            else
               --  For all other tokens, just forward their text as-is

               Append (Buffer (Tok.Source_First .. Tok.Source_Last));
            end if;

         --  Depending on ``Cfg.Line_Mode``, disabled code needs to completely
         --  disappear (``Delete_Lines``), be replaced with blank lines
         --  (``Blank_Lines``) or be forwarded as commented lines
         --  (``Comment_Line``).
         --
         --  In Libadalang, all tokens fit on a single line except the
         --  whitespace token, so whitespaces are the only kind of disabled
         --  token we must handle specially.

         elsif Kind = Ada_Whitespace then
            declare
               Had_Newline : Boolean := not Previous_Enabled;
               --  In ``Delete_Lines`` mode, determine whether we have output
               --  a line feed since the last enabled token. Unused in other
               --  line modes.

               Next : Positive := Tok.Source_First;
               --  In ``Comment_Lines`` mode, index in the original source
               --  buffer of the first codepoint not yet copied to the output.
               --  Used to insert comment prefixes where appropriate. Unused in
               --  other line modes.
            begin
               --  Process each codepoint in this whitespace token separately
               --  to correctly handle new lines.

               for I in Tok.Source_First .. Tok.Source_Last loop
                  if Buffer (I) = Chars.LF then
                     case Cfg.Line_Mode is
                        when Delete_Lines =>

                           --  Make sure that all lines that contain
                           --  disabled tokens are removed from the output, and
                           --  that remaining lines are still separated by line
                           --  feeds.

                           if not Had_Newline then
                              Append (String'(1 => ASCII.LF));
                              Had_Newline := True;
                           end if;

                        when Blank_Lines =>
                           Append (String'(1 => ASCII.LF));

                        when Comment_Lines =>

                           Append (Buffer (Next .. I));

                           --  Do not add a prefix when processing a line feed
                           --  character that ends the file, as there is no
                           --  line after it.

                           if I < Buffer'Last then
                              Append (Comment_Prefix);
                           end if;
                     end case;
                     Next := I + 1;

                  elsif Cfg.Line_Mode = Comment_Lines then
                     Append (Buffer (I .. I));
                  end if;
               end loop;
            end;

         elsif Cfg.Line_Mode = Comment_Lines then
            Append (Buffer (Tok.Source_First .. Tok.Source_Last));
         end if;

         Previous_Enabled := Enabled;
         Cur := Next (Cur, TDH);
      end loop;
   end Emit_Tokens;

   -----------------------
   -- Process_Prep_Line --
   -----------------------

   procedure Process_Prep_Line
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      Text        : Text_Type;
      Sloc        : Source_Location;
      Prev_States : in out Branch_State_Vectors.Vector;
      State       : in out Branch_State;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      --  Parse the directive line using the dedicated parsing rule

      Text_As_Bytes : constant String (1 .. Text'Length * 4)
        with Import, Address => Text'Address;
      U             : constant Analysis_Unit := Context.Get_From_Buffer
        (Filename => "<buffer>",
         Charset  => Text_Charset,
         Buffer   => Text_As_Bytes,
         Rule     => Pp_Directive_Rule);

      function Eval (D : Expr) return Eval_State;
      --  Evaluate the given preprocessing directive controlling expression.
      --  Return ``On`` if the expression evaluates to "true", and ``Off`` if
      --  it evaluates to "false".
      --
      --  If the evaluation aborts because of an error, this appends the
      --  corresponding entry to ``Diagnostics`` and just returns ``Off``.

      type Eval_Value_Kind is (String_Value, Integer_Value);
      type Eval_Value is record

         --  In principle, we would like ``Kind`` to be a discriminant that
         --  determines whether ``Str_Val`` or ``Int_Val`` are components, but
         --  we want to keep the kind mutable: ``Int_Val`` is limited, so Ada
         --  prevents us from using a discriminant.

         Kind    : Eval_Value_Kind;
         Str_Val : US.Unbounded_String;
         Int_Val : Big_Integer;
      end record;
      --  Value for the evaluation of a preprocessing directive sub-expression

      Eval_Error : exception;
      --  Exception used to abort evaluation recursion when an evaluation error
      --  happens. The only handler for this exception is in the ``Eval``
      --  function overload returning ``Eval_State``, which is the entry point
      --  for preprocessing directive condition evaluation (i.e. always at the
      --  top of the recursion).

      procedure Abort_If_Null (D : Expr'Class);
      --  Raise an ``Eval_Error`` if ``D`` is null, do nothing otherwise

      procedure Error (N : Ada_Node'Class; Message : String) with No_Return;
      --  Append an entry to ``Diagnostics`` using ``N``'s sloc range and the
      --  given ``Message``, then raise an ``Eval_Error`` exception.

      type Bool_Op_Kind is (Op_And, Op_Or, Op_Not);
      function Image (Kind : Bool_Op_Kind) return String
      is (case Kind is
          when Op_And => "and",
          when Op_Or  => "or",
          when Op_Not => "not");

      procedure Reject_Bool_Op_Mixing (D : Expr; Expected : Bool_Op_Kind);
      --  If ``D`` is a boolean operation that does not match ``Expected``,
      --  stop with the corresponding error.
      --
      --  This is used to make sure that different boolean operators are not
      --  used together without parens.

      function Eval (D : Expr) return Boolean;
      --  Evaluate the given preprocessing directive controlling expression and
      --  return its result as a boolean. Propagates an ``Eval_Error``
      --  exception in case of evaluation error.

      function Eval_Bool_Op (BO : Bin_Op) return Boolean;
      --  Helper for ``Eval`` specifically for boolean binary operations

      procedure Decode_Value (D : Expr; V : out Eval_Value);
      --  Assuming ``D`` is a single-token expression encoding a preprocessing
      --  value, set ``V`` to that value.

      procedure Get_Symbol_Value
        (Id       : Identifier;
         Kind     : Eval_Value_Kind;
         V        : out Eval_Value;
         Or_False : Boolean := False)
      with Pre => not Or_False or else Kind = String_Value;
      --  Get the value for a given preprocessing symbol.
      --
      --  ``Id`` is the identifier that designates the symbol to fetch, and
      --  ``Kind`` designates the expected value kind for that symbol.
      --
      --  When returning ``V`` is set to the decoded symbol value.
      --
      --  If ``Or_False`` is true *and* the undefined-is-false configuration is
      --  enabled, set ``V`` to the ``false`` string when the symbol is
      --  undefined (this is valid iff. a string is requested).

      function Decode_Symbol (Id : Identifier) return US.Unbounded_String;
      --  Turn an identifier into the corresponding preprocessor symbol
      --  representation.

      function Outer_Sloc (S : Source_Location) return Source_Location;
      --  Convert ``S`` (a source location relative to ``Text``) to the
      --  corresponding location in the original source buffer.

      function Outer_Sloc_Range
        (SR : Source_Location_Range) return Source_Location_Range
      is (Make_Range (Outer_Sloc (Start_Sloc (SR)),
                      Outer_Sloc (End_Sloc (SR))));
      --  Likewise, but for source location ranges

      ----------
      -- Eval --
      ----------

      function Eval (D : Expr) return Eval_State is
      begin
         return (if Eval (D) then On else Off);
      exception
         when Eval_Error =>
            return Off;
      end Eval;

      -------------------
      -- Abort_If_Null --
      -------------------

      procedure Abort_If_Null (D : Expr'Class) is
      begin
         if D.Is_Null then
            raise Eval_Error;
         end if;
      end Abort_If_Null;

      -----------
      -- Error --
      -----------

      procedure Error (N : Ada_Node'Class; Message : String) is
      begin
         Append
           (Diagnostics, Outer_Sloc_Range (N.Sloc_Range), To_Text (Message));
         raise Eval_Error;
      end Error;

      ---------------------------
      -- Reject_Bool_Op_Mixing --
      ---------------------------

      procedure Reject_Bool_Op_Mixing (D : Expr; Expected : Bool_Op_Kind) is
         BO     : Bin_Op;
         Op     : Libadalang.Analysis.Op;
         Actual : Bool_Op_Kind;
      begin
         if D.Is_Null then
            return;
         end if;

         --  Determine the binary operator in ``D``, or return if it is not a
         --  binary operation.

         case D.Kind is
            when Ada_Bin_Op =>
               BO := D.As_Bin_Op;
               if BO.F_Op.Is_Null then
                  return;
               end if;
               Op := BO.F_Op;
               case BO.F_Op.Kind is
                  when Ada_Op_And | Ada_Op_And_Then =>
                     Actual := Op_And;
                  when Ada_Op_Or | Ada_Op_Or_Else =>
                     Actual := Op_Or;
                  when others =>
                     return;
               end case;

            when Ada_Un_Op =>

               --  The only kind of unary operator that grammar rules used to
               --  parse expressions in this context.

               Actual := Op_Not;
               Op := D.As_Un_Op.F_Op;

            when others =>
               return;
         end case;

         if Expected /= Actual then
            Error (Op, "mixing """ & Image (Expected) & """ and """
                       & Image (Actual) & """ is not allowed");
         end if;
      end Reject_Bool_Op_Mixing;

      ----------
      -- Eval --
      ----------

      function Eval (D : Expr) return Boolean is
      begin
         Abort_If_Null (D);
         case D.Kind is
            when Ada_Paren_Expr =>
               return Eval (D.As_Paren_Expr.F_Expr);

            when Ada_Identifier =>

               --  Evaluate a symbol as a boolean

               declare
                  Value : Eval_Value;
               begin
                  --  If the symbol is undefined and option "-u" is used,
                  --  consider it's "false". Consider it as an error if "-u" is
                  --  not used.

                  begin
                     Get_Symbol_Value
                       (D.As_Identifier,
                        String_Value,
                        Value,
                        Or_False => True);
                  exception
                     when Eval_Error =>
                        if Cfg.Undefined_Is_False then
                           return False;
                        else
                           raise;
                        end if;
                  end;

                  --  Check that the symbol is either to "true" or "false"
                  --  (case insensitive). Return the corresponding Boolean
                  --  value.

                  declare
                     Text : constant String :=
                       To_Lower (US.To_String (Value.Str_Val));
                  begin
                     if Text in "" | "true" then
                        return True;
                     elsif Text = "false" then
                        return False;
                     else
                        Error (D, "value of symbol "
                                  & Image (D.Text, With_Quotes => True)
                                  & " is not True or False");
                     end if;
                  end;
               end;

            when Ada_Attribute_Ref =>

               --  Expect a "<symbol>'Defined" construct, to return if the
               --  referenced symbol is defined. Note that attribute references
               --  that come from the preprocessor directive parsing rules
               --  always have an identifier prefix.

               declare
                  AR   : constant Attribute_Ref := D.As_Attribute_Ref;
                  Attr : Identifier;
               begin
                  Abort_If_Null (AR.F_Prefix);
                  Abort_If_Null (AR.F_Attribute);
                  Attr := AR.F_Attribute;

                  if To_Lower (Image (Attr.Text)) /= "defined" then
                     Error (Attr, "identifier ""Defined"" expected");
                  end if;

                  return Cfg.Definitions.Contains
                           (Decode_Symbol (AR.F_Prefix.As_Identifier));
               end;

            when Ada_Un_Op =>
               declare
                  UO : constant Un_Op := D.As_Un_Op;
               begin
                  Abort_If_Null (UO.F_Op);

                  --  The grammar rule used to parse expressions in this
                  --  context is not supposed to create other kind of nodes.

                  pragma Assert (UO.F_Op.Kind = Ada_Op_Not);

                  Reject_Bool_Op_Mixing (UO.F_Expr, Op_Not);
                  return not Eval (UO.F_Expr);
               end;

            when Ada_Bin_Op_Range =>
               declare
                  use type GNATCOLL.GMP.Integers.Big_Integer;
                  use type US.Unbounded_String;

                  BO : constant Bin_Op := D.As_Bin_Op;
                  Left, Right : Eval_Value;
               begin
                  Abort_If_Null (BO.F_Op);

                  --  Boolean operators need special processing

                  if BO.F_Op.Kind in Ada_Op_And | Ada_Op_And_Then | Ada_Op_Or
                                     | Ada_Op_Or_Else
                  then
                     return Eval_Bool_Op (BO);
                  end if;

                  --  All relational binary operations compare the value of a
                  --  symbol (left) to some other value (right). How we must
                  --  interpret the value of the symbol depends on what the
                  --  value on the right, so evaluate the latter first...

                  Decode_Value (BO.F_Right, Right);
                  Get_Symbol_Value (BO.F_Left.As_Identifier, Right.Kind, Left);

                  --  The only operator that works for both strings and
                  --  integers is the equality. All other operators require
                  --  integers.

                  if BO.F_Op.Kind = Ada_Op_Eq then
                     return
                       Left.Kind = Right.Kind
                         and then
                       (case Left.Kind is
                        when String_Value  => Left.Str_Val = Right.Str_Val,
                        when Integer_Value => Left.Int_Val = Right.Int_Val);

                  elsif Left.Kind /= Integer_Value then
                     Error (BO.F_Left, "integer expected");
                  end if;

                  case BO.F_Op.Kind is
                     when Ada_Op_Lt  => return Left.Int_Val < Right.Int_Val;
                     when Ada_Op_Lte => return Left.Int_Val <= Right.Int_Val;
                     when Ada_Op_Gt  => return Left.Int_Val > Right.Int_Val;
                     when Ada_Op_Gte => return Left.Int_Val >= Right.Int_Val;

                     --  The grammar rule used to parse expressions in this
                     --  context is not supposed to create other kind of nodes.

                     when others => return (raise Program_Error);
                  end case;
               end;

            --  The grammar rule used to parse expressions in this context is
            --  not supposed to create other kind of nodes.

            when others =>
               return (raise Program_Error);
         end case;
      end Eval;

      ------------------
      -- Eval_Bool_Op --
      ------------------

      function Eval_Bool_Op (BO : Bin_Op) return Boolean is
         Short_Circuit : Boolean := False;
         --  Whether the operator we are evaluating is short-circuiting

         Has_And : Boolean := False;
         --  Whether ``BO`` is the "and" or "and then" operator

         Left, Right : Boolean;
      begin
         Abort_If_Null (BO.F_Op);

         --  Match the operator to evaluate

         case BO.F_Op.Kind is
            when Ada_Op_And =>
               Has_And := True;
            when Ada_Op_And_Then =>
               Has_And := True;
               Short_Circuit := True;
            when Ada_Op_Or =>
               null;
            when Ada_Op_Or_Else =>
               Short_Circuit := True;

            --  The grammar rule used to parse expressions in this
            --  context is not supposed to create other kind of nodes.

            when others => raise Program_Error;
         end case;

         --  First evaluate the left operand

         Reject_Bool_Op_Mixing
           (BO.F_Left, (if Has_And then Op_And else Op_Or));
         Left := Eval (BO.F_Left);

         --  Return early if we have "False and then" or "True or else"

         if Short_Circuit and then (if Has_And then not Left else Left) then
            return Left;
         end if;

         --  Finally evaluate the right operand and compute the result

         Reject_Bool_Op_Mixing
           (BO.F_Right, (if Has_And then Op_And else Op_Or));
         Right := Eval (BO.F_Right);

         return (if Has_And
                 then Left and Right
                 else Left or Right);
      end Eval_Bool_Op;

      ------------------
      -- Decode_Value --
      ------------------

      procedure Decode_Value (D : Expr; V : out Eval_Value) is
      begin
         Abort_If_Null (D);
         case D.Kind is
            when Ada_String_Literal =>
               V.Kind := String_Value;
               V.Str_Val := US.To_Unbounded_String
                 (Image (D.As_String_Literal.P_Denoted_Value));

            when Ada_Int_Literal =>
               V.Kind := Integer_Value;
               V.Int_Val.Set (D.As_Int_Literal.P_Denoted_Value);

            when Ada_Identifier =>
               Get_Symbol_Value (D.As_Identifier, String_Value, V);

            --  The grammar rule used to parse expressions in this
            --  context is not supposed to create other kind of nodes.

            when others =>
               raise Program_Error;
         end case;
      end Decode_Value;

      ----------------------
      -- Get_Symbol_Value --
      ----------------------

      procedure Get_Symbol_Value
        (Id       : Identifier;
         Kind     : Eval_Value_Kind;
         V        : out Eval_Value;
         Or_False : Boolean := False)
      is
         use Definition_Maps;
         Cur : Cursor;
      begin
         Abort_If_Null (Id);

         --  Look for a definition for the requested symbol and fetch its value

         Cur := Cfg.Definitions.Find (Decode_Symbol (Id));
         if not Has_Element (Cur) then
            if Or_False and then Cfg.Undefined_Is_False then
               V.Kind := String_Value;
               V.Str_Val := US.To_Unbounded_String ("False");
               return;
            else
               Error
                 (Id,
                  "unknown symbol " & Image (Id.Text, With_Quotes => True));
            end if;
         end if;

         --  Convert that value to the requested eval value kind

         declare
            Str_Value : constant US.Unbounded_String :=
              As_String (Element (Cur));
         begin
            V.Kind := Kind;
            case Kind is
               when String_Value =>
                  V.Str_Val := Str_Value;
               when Integer_Value =>
                  V.Int_Val.Set (US.To_String (Str_Value));
            end case;
         exception
            when GNATCOLL.GMP.Integers.Failure =>
               Error
                 (Id,
                  "symbol " & Image (Id.Text, With_Quotes => True)
                  & " value is not an integer");
         end;
      end Get_Symbol_Value;

      -------------------
      -- Decode_Symbol --
      -------------------

      function Decode_Symbol (Id : Identifier) return US.Unbounded_String is
         Sym_Text : constant Text_Type := Id.Text;
      begin
         if Sym_Text (Sym_Text'First) = '$' then
            Error (Id, "unexpected '$' in preprocessing directive");
         end if;
         return US.To_Unbounded_String (To_Lower (Image (Sym_Text)));
      end Decode_Symbol;

      ----------------
      -- Outer_Sloc --
      ----------------

      function Outer_Sloc (S : Source_Location) return Source_Location is
      begin
         pragma Assert (S.Line = 1);
         return (Sloc.Line, Sloc.Column + S.Column - 1);
      end Outer_Sloc;

   begin
      --  Forward parsing diagnostics to the preprocessors'

      for D of U.Diagnostics loop
         Diagnostics.Append
           (Diagnostic'(Outer_Sloc_Range (D.Sloc_Range), D.Message));
      end loop;

      --  Try to keep running even if parsing partially failed

      if U.Root.Is_Null then
         return;
      end if;

      case Ada_Pp_Directive (U.Root.Kind) is
         when Ada_Pp_If_Directive =>

            --  ``#if`` directives always start a new sequence: push the
            --  current state to ``Prev_States`` for later and create a fresh
            --  state for this new sequence in ``State``.
            --
            --  This new sequence can be ``On``/``Off`` only if the block that
            --  contains this directive is enabled: if not, the whole nested
            --  sequence must be disabled.

            Prev_States.Append (State);
            State :=
              (Start_Line    => Sloc.Line,
               State         =>
                 (if State.State = On
                  then Eval (U.Root.As_Pp_If_Directive.F_Expr)
                  else Disabled),
               Last_Was_Else => False);

         when Ada_Pp_Elsif_Directive =>
            if Prev_States.Is_Empty then
               Append (Diagnostics, Sloc, "no matching ""#if""");

            elsif State.Last_Was_Else then
               Append (Diagnostics, Sloc, "duplicate ""#else""");

            --  If the block was enabled, no other block in this sequence can
            --  be enabled: disable branch evaluation.

            elsif State.State = On then
               State.State := Disabled;

            --  If no block has been enabled in this sequence so far, evaluate
            --  this condition to determine if the next block is enabled.

            elsif State.State = Off then
               State.State := Eval (U.Root.As_Pp_Elsif_Directive.F_Expr);
            end if;

         when Ada_Pp_Else_Directive =>
            if Prev_States.Is_Empty then
               Append (Diagnostics, Sloc, "no matching ""#if""");

            elsif State.Last_Was_Else then
               Append (Diagnostics, Sloc, "duplicate ""#else""");

            --  If no previous block in this branch was enabled, the block
            --  after this ``#else`` directive is enabled. In all other cases,
            --  it is disabled.

            else
               State.State := (if State.State = Off then On else Disabled);
            end if;

            State.Last_Was_Else := True;

         when Ada_Pp_End_If_Directive =>

            --  Make sure this directive has a sequence to close, and restore
            --  the previously saved evaluation state.

            if Prev_States.Is_Empty then
               Append (Diagnostics, Sloc, "no matching ""#if""");
            else
               State := Prev_States.Last_Element;
               Prev_States.Delete_Last;
            end if;
      end case;
   end Process_Prep_Line;

   ----------------
   -- Preprocess --
   ----------------

   procedure Preprocess
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      Input       : String;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
      Buffer : Text_Access;
      --  Buffer to hold the decoded input source (i.e. ``Input`` converted
      --  from ``String`` to ``Text_Type``).

      TDH : Token_Data_Handler;
      --  Holder for all tokens in the original source

      Syms : Symbol_Table;
      --  Symbol table for ``TDH``

      Same_Contents : Boolean;
   begin
      --  If preprocessing is disabled for this file, just copy the input to
      --  the output.

      if not Cfg.Enabled then
         Contents.Buffer := new String (1 .. Input'Length);
         Contents.Last := Input'Length;
         Contents.Buffer.all := Input;
         return;
      end if;

      --  Convert the input bytes buffer into a text buffer so that
      --  Libadalang's lexer can process it.

      declare
         Idx : Positive := Input'First;
      begin
         Buffer := new Text_Type (1 .. Input'Length);
         for I in Buffer'Range loop
            Buffer (I) := Character_Type'Val (Character'Pos (Input (Idx)));
            Idx := Idx + 1;
         end loop;
      end;

      --  Split the source buffer into Ada tokens

      Syms := Create_Symbol_Table;
      Initialize (TDH, Syms, System.Null_Address);
      Extract_Tokens
        (Input         => (Text_Buffer, Buffer.all'Address, Buffer'Length),
         With_Trivia   => True,
         File_Reader   => null,
         TDH           => TDH,
         Diagnostics   => Diagnostics,
         Old_TDH       => null,
         Same_Contents => Same_Contents);
      pragma Assert (not Same_Contents);

      --  Go through TDH's tokens and create a list of tokens to emit in the
      --  returned source buffer.

      declare
         Tokens : Enabled_Tokens := new Enabled_Tokens_Array
           (1 .. TDH.Tokens.Length + TDH.Trivias.Length);
      begin
         Process_Tokens (Cfg, Context, TDH, Tokens.all, Diagnostics);

         --  Now go through these tokens and fill the buffer to return

         Emit_Tokens (Cfg, TDH, Tokens.all, Contents, Diagnostics);

         Free (Tokens);
      end;

      --  Our job is now done: clean up local resources and return

      Free (TDH);
      Free (Buffer);
      Destroy (Syms);
   end Preprocess;

end Libadalang.PP_Impl;
