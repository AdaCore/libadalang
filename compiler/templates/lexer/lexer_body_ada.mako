## vim: filetype=makoada

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with System;

with GNATCOLL.Symbols; use GNATCOLL.Symbols;

package body ${_self.ada_api_settings.lib_name}.Lexer is

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

   function Lexer_From_Filename (Filename, Charset : chars_ptr)
                                 return Lexer_Type
      with Import        => True,
           Convention    => C,
           External_Name => "${capi.get_name("lexer_from_filename")}";

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

   procedure Process_All_Tokens (Lexer : Lexer_Type;
                                 TDH   : in out Token_Data_Handler) is
      Token    : aliased Token_Type;
      Text     : String_Access;
      Continue : Boolean := True;

      function Bounded_Text return String is
        (Value (Token.Text, Token.Text_Length));

   begin
      while Continue loop
         Continue := Next_Token (Lexer, Token'Unrestricted_Access) /= 0;

         --  Next_Token returns 0 for the last token, which will be our "null"
         --  token.

         --  TODO??? This is specific to Ada and should be defined in the DSL

         if Token.Id = QUEX_TKN_STRING then
            Text := Add_String (TDH, Bounded_Text);
         elsif Token.Id in QUEX_TKN_IDENTIFIER | QUEX_TKN_LABEL | QUEX_TKN_CHAR
                           | QUEX_TKN_NUMBER | QUEX_TKN_NULL
         then
            --  TODO??? GNATCOLL.Symbol forces us to work with Symbol values.
            --  These are accesses to unconstrained arrays but we want to work
            --  with Ada.Strings.Unbounded.String_Access values...
            Text := Convert (Find (TDH.Symbols, Bounded_Text));
         else
            Text := null;
         end if;

         TDH.Tokens.Append
           ((Id   => Token.Id,
             Text => Text,
             Sloc_Range => (Token.Start_Line,   Token.End_Line,
                            Token.Start_Column, Token.End_Column)));
      end loop;
   end Process_All_Tokens;

   -----------------------
   -- Lex_From_Filename --
   -----------------------

   procedure Lex_From_Filename
     (Filename, Charset : String;
      TDH               : in out Token_Data_Handler)
   is
      Filename_Ptr : chars_ptr := New_String (Filename);
      Charset_Ptr  : chars_ptr :=
        (if Charset = "" then Null_Ptr else New_String (Charset));

      Lexer    : Lexer_Type := Lexer_From_Filename (Filename_Ptr, Charset_Ptr);
   begin
      Process_All_Tokens (Lexer, TDH);
      Free_Lexer (Lexer);
      Free (Filename_Ptr);
      Free (Charset_Ptr);
   end Lex_From_Filename;

   ---------------------
   -- Lex_From_Buffer --
   ---------------------

   procedure Lex_From_Buffer (Buffer : String;
                              TDH    : in out Token_Data_Handler) is
      Buffer_Ptr : System.Address := Buffer'Address;
      Lexer      : Lexer_Type :=
         Lexer_From_Buffer (Buffer_Ptr, Buffer'Length);
   begin
      Process_All_Tokens (Lexer, TDH);
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
       % for tok_name, _ in token_map.tokens.items():
           (Id   => ${token_map.TOKEN_PREFIX + tok_name},
            Text => To_Unbounded_String
              ("${token_map.names_to_str.get(tok_name, tok_name.lower())}"))
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
