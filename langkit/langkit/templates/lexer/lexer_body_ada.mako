## vim: filetype=makoada

with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Unchecked_Conversion;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;

with GNATCOLL.Iconv;
with GNATCOLL.Mmap;    use GNATCOLL.Mmap;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Text;    use Langkit_Support.Text;

package body ${_self.ada_api_settings.lib_name}.Lexer is

   use Token_Vectors, Trivia_Vectors, Integer_Vectors;

   type Token_Type is record
      Id                       : Unsigned_16;
      Text                     : System.Address;
      Text_Length              : size_t;
      Start_Line, End_Line     : Unsigned_32;
      Start_Column, End_Column : Unsigned_16;
   end record
      with Convention => C;
   type Token_Access is access all Token_Type;

   type Lexer_Type is new System.Address;

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Decoded_Buffer  : out Text_Access;
      Length          : out Natural);
   --  Allocate a Text_Type buffer, set it to Decoded_Buffer, decode Buffer
   --  into it using Charset and set Length to the number of decoded characters
   --  in Decoded_Buffer. It is up to the caller to deallocate Decoded_Buffer
   --  when done with it.
   --
   --  Quex quirk: this actually allocates more than the actual buffer to keep
   --  Quex happy. The two first characters are set to null and there is an
   --  extra null character at the end of the buffer.

   function Lexer_From_Buffer (Buffer  : System.Address;
                               Length  : Size_T)
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
      Text                  : Text_Access;
      Continue              : Boolean := True;
      Last_Token_Was_Trivia : Boolean := False;

      function Bounded_Text return Text_Type;
      --  Return a copy of the text in Token.  Do not call this if the token
      --  has no text associated.

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
            --  current token.
            Last_Token_Was_Trivia := False;
         end if;
      end Prepare_For_Trivia;

      ------------------
      -- Bounded_Text --
      ------------------

      function Bounded_Text return Text_Type
      is
         Length : constant Natural := Natural (Token.Text_Length);
         Buffer : Text_Type (1 .. Length);
         for Buffer'Address use Token.Text;
      begin
         return Buffer;
      end Bounded_Text;

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
            ## internalize the text.
            when ${" | ".join(
               get_context().lexer.token_name(tok)
               for tok in get_context().lexer.token_actions['WithSymbol']
            )} =>
               Text := Text_Access (Find (TDH.Symbols, Bounded_Text));

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
      --  The following call to Open_Read may fail with a Name_Error exception:
      --  just let it propagate to the caller as there is no resource to
      --  release yet here.

      File        : Mapped_File := Open_Read (Filename);

      Region      : Mapped_Region := Read (File);
      Buffer_Addr : constant System.Address := Data (Region).all'Address;

      Buffer      : String (1 .. Last (Region));
      for Buffer'Address use Buffer_Addr;

   begin
      begin
         Lex_From_Buffer (Buffer, Charset, TDH, With_Trivia);
      exception
         when Unknown_Charset | Invalid_Input =>
            Free (Region);
            Close (File);
            raise;
      end;
      Free (Region);
      Close (File);
   end Lex_From_Filename;

   ---------------------
   -- Lex_From_Buffer --
   ---------------------

   procedure Lex_From_Buffer (Buffer, Charset : String;
                              TDH             : in out Token_Data_Handler;
                              With_Trivia     : Boolean)
   is
      Decoded_Buffer : Text_Access;
      Length         : Natural;
      Lexer          : Lexer_Type;
   begin
      Decode_Buffer (Buffer, Charset, Decoded_Buffer, Length);
      Lexer := Lexer_From_Buffer (Decoded_Buffer.all'Address, size_t (Length));
      if With_Trivia then
         Process_All_Tokens_With_Trivia (Lexer, TDH);
      else
         Process_All_Tokens_No_Trivia (Lexer, TDH);
      end if;
      Free_Lexer (Lexer);
      Free (Decoded_Buffer);
   end Lex_From_Buffer;

   -------------------
   -- Decode_Buffer --
   -------------------

   procedure Decode_Buffer
     (Buffer, Charset : String;
      Decoded_Buffer  : out Text_Access;
      Length          : out Natural)
   is
      use GNATCOLL.Iconv;

      --  In the worst case, we have one character per input byte, so the
      --  following is supposed to be big enough.

      Result : Text_Access := new Text_Type (1 .. Buffer'Length + 3);
      State  : Iconv_T;
      Status : Iconv_Result;

      Input_Index, Output_Index : Positive;

      First_Output_Index : constant Positive := 1 + 2 * 4;
      --  Index of the first byte in Output at which Iconv must decode Buffer

      Output : Byte_Sequence (1 .. 4 * Buffer'Size);
      for Output'Address use Result.all'Address;
      --  Iconv works on mere strings, so this is a kind of a view conversion

   begin
      Decoded_Buffer := Result;

      --  GNATCOLL.Iconv raises a Constraint_Error for empty strings: handle
      --  them here.

      if Buffer'Length = 0 then
         Length := 0;
         return;
      end if;

      --  Create the Iconv converter. We will notice unknown charsets here

      declare
         use System;

         To_Code : constant String :=
           (if Default_Bit_Order = Low_Order_First
            then UTF32LE
            else UTF32BE);
      begin
         State := Iconv_Open (To_Code, Charset);
      exception
         when Unsupported_Conversion =>
            Free (Result);
            raise Unknown_Charset;
      end;

      --  Perform the conversion itself

      Input_Index := Buffer'First;
      Output_Index := First_Output_Index;
      Iconv (State,
             Buffer, Input_Index,
             Output (Output_Index .. Output'Last), Output_Index,
             Status);

      --  Raise an error if the input was invalid

      case Status is
         when Invalid_Multibyte_Sequence | Incomplete_Multibyte_Sequence =>
            --  TODO??? It may be more helpful to actually perform lexing on an
            --  incomplete buffer. The user would get both a diagnostic for the
            --  charset error and a best-effort list of tokens.

            Free (Result);
            Iconv_Close (State);
            raise Invalid_Input;

         when Full_Buffer =>

            --  This is not supposed to happen: we allocated Result to be big
            --  enough in all cases.

            raise Program_Error;

         when Success =>
            null;
      end case;

      --  Clear the bytes we left for Quex

      declare
         Nul : constant Wide_Wide_Character := Wide_Wide_Character'Val (0);
      begin
         Result (1) := Nul;
         Result (2) := Nul;
         Result (Buffer'Length + 3) := Nul;
      end;

      Iconv_Close (State);
      Length := (Output_Index - First_Output_Index) / 4;
   end Decode_Buffer;

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
