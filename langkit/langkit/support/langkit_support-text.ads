with Ada.Unchecked_Deallocation;

package Langkit_Support.Text is

   subtype Text_Type is Wide_Wide_String;
   --  All our strings are encoded in UTF-32 (native endinannness). This type,
   --  which is not a subtype of String, makes it obvious when conversions are
   --  needed.

   function To_Text (S : String) return Text_Type;
   --  Convenience converter for pure ASCII strings. Raise a Constraint_Error
   --  if a non-ASCII character is met.

   function Image
     (T : Text_Type; With_Quotes : Boolean := False)
      return String;
   --  Return a Python-style quoted string for T. If With_Quote is false, do
   --  not output the boundary quotes.

   type Text_Access is access all Text_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Text_Type, Text_Access);

end Langkit_Support.Text;
