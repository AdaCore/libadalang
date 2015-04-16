## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Interfaces;           use Interfaces;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with Liblang_Support.Token_Data_Handler; use Liblang_Support.Token_Data_Handler;

package ${_self.ada_api_settings.lib_name}.Lexer is

   procedure Lex_From_Filename (Filename, Charset : String;
                                TDH               : in out Token_Data_Handler);

   procedure Lex_From_Buffer (Buffer : String;
                              TDH    : in out Token_Data_Handler);

   function Token_Text (Token_Id : Unsigned_16) return String;

   ## When generated code needs to deal with token kinds, it could use integer
   ## literals but this would not be convenient to read. Generate instead named
   ## constants for each token kind.

   % for tok_name, tok_value in sorted(token_map.tokens.items(), key=lambda (tn, tv): tv):
       ${token_map.TOKEN_PREFIX + tok_name} : constant := ${tok_value};
   % endfor

private

   function Hash (N : Unsigned_16) return Ada.Containers.Hash_Type is
     (Ada.Containers.Hash_Type (N));

   package Token_Text_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unsigned_16,
      Element_Type    => Unbounded_String,
      Hash            => Hash,
      Equivalent_Keys => "=");

   Token_Text_Map : Token_Text_Maps.Map;

   function Token_Text (Token_Id : Unsigned_16) return String is
     (To_String (Token_Text_Map.Element (Token_Id)));

end ${_self.ada_api_settings.lib_name}.Lexer;
