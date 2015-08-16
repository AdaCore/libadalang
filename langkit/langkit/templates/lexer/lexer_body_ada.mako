## vim: filetype=makoada

with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;

with GNATCOLL.Mmap;    use GNATCOLL.Mmap;
with GNATCOLL.Symbols; use GNATCOLL.Symbols;

package body ${_self.ada_api_settings.lib_name}.Lexer is

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   type Token_Type is record
      Id                       : Unsigned_16;
      Text                     : chars_ptr;
      Text_Length              : size_t;
      Start_Line, End_Line     : Unsigned_32;
      Start_Column, End_Column : Unsigned_16;
   end record
      with Convention => C;
   type Token_Access is access all Token_Type;

   type Lexer_Type is new System.Address;

   function Lexer_From_Buffer (Buffer : System.Address;
                               Length : Size_T)
                               return Lexer_Type
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("lexer_from_buffer")}";

   procedure Free_Lexer (Lexer : Lexer_Type)
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("free_lexer")}";

   function Next_Token (Lexer : Lexer_Type;
                        Token : Token_Access) return int
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("next_token")}";

   function Convert is new Ada.Unchecked_Conversion (Symbol, String_Access);

   ------------------------
   -- Process_All_Tokens --
   ------------------------

   generic
      With_Trivia : Boolean;
   procedure Process_All_Tokens (Lexer : Lexer_Type;
                                 TDH   : in out Token_Data_Handler);

   procedure Process_All_Tokens (Lexer : Lexer_Type;
                                 TDH   : in out Token_Data_Handler)
   is

      Token                 : aliased Token_Type;
      Text                  : String_Access;
      Continue              : Boolean := True;
      Last_Token_Was_Trivia : Boolean := False;

      function Bounded_Text return String is
        (Value (Token.Text, Token.Text_Length));

      procedure Prepare_For_Trivia
        with Inline_Always;
      --  Append an entry for the current token in the Tokens_To_Trivias
      --  correspondence vector.

      procedure Prepare_For_Trivia is
      begin
         if With_Trivia then
            --  By default, the current token will have no trivia
            Append (TDH.Tokens_To_Trivias, -1);

            --  Reset Last_Token_Was_Trivia so that new trivia is added to the
            --  current token
            Last_Token_Was_Trivia := False;
         end if;
      end Prepare_For_Trivia;
   begin
      --  In the case we are reparsing an analysis unit, we want to get rid of
      --  the tokens from the old one.

      Reset (TDH);

      --  The first entry in the Tokens_To_Trivias map is for leading trivias
      Prepare_For_Trivia;

      while Continue loop

         --  Next_Token returns 0 for the last token, which will be our "null"
         --  token.

         Continue := Next_Token (Lexer, Token'Unrestricted_Access) /= 0;

         case Token.Id is

         % if get_context().lexer.token_actions['WithText']:
            ## Token id is part of the class of token types for which we want to
            ## keep the text, but without internalization of the text.
            when ${" | ".join(
               get_context().lexer.token_name(tok)
               for tok in get_context().lexer.token_actions['WithText']
            )} =>
               Text := Add_String (TDH, Bounded_Text);

               Prepare_For_Trivia;

         % endif

         % if get_context().lexer.token_actions['WithSymbol']:
            ## Token id is part of the class of token types for which we want to
            ## internalize the text
            when ${" | ".join(
               get_context().lexer.token_name(tok)
               for tok in get_context().lexer.token_actions['WithSymbol']
            )} =>
               --  TODO??? GNATCOLL.Symbol forces us to work with Symbol values.
               --  These are accesses to unconstrained arrays but we want to work
               --  with Ada.Strings.Unbounded.String_Access values...

               Text := Convert (Find (TDH.Symbols, Bounded_Text));

               Prepare_For_Trivia;
         % endif

         % if get_context().lexer.token_actions['WithTrivia']:
            when ${" | ".join(
               get_context().lexer.token_name(tok)
               for tok in get_context().lexer.token_actions['WithTrivia']
            )} =>
               if With_Trivia then
                  Text := Add_String (TDH, Bounded_Text);

                  if Last_Token_Was_Trivia then
                     Last_Element (TDH.Trivias).all.Has_Next := True;
                  else
                     Last_Element (TDH.Tokens_To_Trivias).all := Length (TDH.Trivias);
                  end if;

                  Append
                    (TDH.Trivias,
                      (Has_Next => False,
                       T        => (Id   => Token.Id,
                                    Text => Text,
                                    Sloc_Range =>
                                      (Token.Start_Line,   Token.End_Line,
                                       Token.Start_Column, Token.End_Column))));

                  Last_Token_Was_Trivia := True;
               end if;

               goto Dont_Append;
         % endif

            ## Else, don't keep the text at all
            when others =>
               Text := null;
               Prepare_For_Trivia;

         end case;

         Append
           (TDH.Tokens,
            (Id   => Token.Id,
             Text => Text,
             Sloc_Range => (Token.Start_Line,   Token.End_Line,
                            Token.Start_Column, Token.End_Column)));

         <<Dont_Append>>
      end loop;

   end Process_All_Tokens;

   procedure Process_All_Tokens_With_Trivia is new Process_All_Tokens (True);
   procedure Process_All_Tokens_No_Trivia is new Process_All_Tokens (False);

   -----------------------
   -- Lex_From_Filename --
   -----------------------

   procedure Lex_From_Filename
     (Filename, Charset : String;
      TDH               : in out Token_Data_Handler;
      With_Trivia       : Boolean)
   is
      pragma Unreferenced (Charset);
      --  TODO??? we should handle charset handling at some point

      --  The following call to Open_Read may fail with a Name_Error exception:
      --  just let it propagate to the caller as there is no resource to
      --  release yet here.

      File        : Mapped_File := Open_Read (Filename);

      Region      : Mapped_Region := Read (File);
      Buffer      : constant System.Address := Data (Region).all'Address;
      Buffer_Size : constant size_t := size_t (Last (Region));

      Lexer       : Lexer_Type :=
         Lexer_From_Buffer (Buffer, Buffer_Size);

   begin
      if With_Trivia then
         Process_All_Tokens_With_Trivia (Lexer, TDH);
      else
         Process_All_Tokens_No_Trivia (Lexer, TDH);
      end if;

      Free_Lexer (Lexer);
      Free (Region);
      Close (File);
   end Lex_From_Filename;

   ---------------------
   -- Lex_From_Buffer --
   ---------------------

   procedure Lex_From_Buffer (Buffer      : String;
                              TDH         : in out Token_Data_Handler;
                              With_Trivia : Boolean)
   is
      Buffer_Ptr : System.Address := Buffer'Address;
      Lexer      : Lexer_Type :=
         Lexer_From_Buffer (Buffer_Ptr, Buffer'Length);
   begin
      if With_Trivia then
         Process_All_Tokens_With_Trivia (Lexer, TDH);
      else
         Process_All_Tokens_No_Trivia (Lexer, TDH);
      end if;
      Free_Lexer (Lexer);
   end Lex_From_Buffer;

begin

   declare
      type Entry_Type is record
         Id   : Unsigned_16;
         Text : Unbounded_String;
      end record;

      Entries : constant array (Natural range <>) of Entry_Type :=
        (
       % for tok in get_context().lexer.tokens_class:
           (Id   => ${get_context().lexer.token_name(tok)},
            Text => To_Unbounded_String
              ("${tok.name}"))
           % if (not loop.last):
               ,
           % endif
       % endfor
        );
   begin
      for E of Entries loop
         Token_Text_Map.Insert (E.Id, E.Text);
      end loop;
   end;

end ${_self.ada_api_settings.lib_name}.Lexer;
