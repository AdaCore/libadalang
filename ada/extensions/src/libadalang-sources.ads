with Langkit_Support.Text; use Langkit_Support.Text;

--  Internal unit to deal with sources text decoding

private package Libadalang.Sources is

   Invalid_Brackets_Encoding : exception;

   function Decode_Brackets (Pattern : Text_Type) return Wide_Wide_Character
      with Inline;
   --  Assuming Pattern is a bracket encoding for a character of the form:
   --    ["DIGITS"]
   --  where DIGITS is a sequence of 2, 4, 6 or 8 hexadecimal characters,
   --  return the corresponding character. Raise an Invalid_Brackets_Encoding
   --  if Pattern is invalid.

   function Canonicalize (Name : Text_Type) return Text_Type
      with Inline;
   --  Return a canonicalized name for Name. This performs case folding and
   --  brackets decoding.

end Libadalang.Sources;
