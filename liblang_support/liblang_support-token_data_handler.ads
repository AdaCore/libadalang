with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.Symbols; use GNATCOLL.Symbols;

with Liblang_Support.Tokens; use Liblang_Support.Tokens;

package Liblang_Support.Token_Data_Handler is

   package Token_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Token);
   package String_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => String_Access);

   type Token_Data_Handler is record
      Tokens          : Token_Vectors.Vector;
      Symbols         : Symbol_Table_Access;
      String_Literals : String_Vectors.Vector;
   end record;

   type Token_Data_Handler_Access is access all Token_Data_Handler;

   procedure Initialize (TDH     : out Token_Data_Handler;
                         Symbols : Symbol_Table_Access);

   function Add_String (TDH : in out Token_Data_Handler;
                        S   : String) return String_Access;

   function Get_Token (TDH   : Token_Data_Handler;
                       Index : Natural) return Token is
     (Token_Vectors.Element (TDH.Tokens, Index));

   procedure Free (TDH : in out Token_Data_Handler);

end Liblang_Support.Token_Data_Handler;
