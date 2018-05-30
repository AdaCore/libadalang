with GNATCOLL.GMP.Integers;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Lexer; use Libadalang.Lexer;

--  Internal unit to deal with sources text decoding

private package Libadalang.Sources is

   procedure Decode_Brackets
     (Pattern : Text_Type;
      Error   : out Boolean;
      Result  : out Wide_Wide_Character)
      with Inline;
   --  Assuming Pattern is a bracket encoding for a character of the form:
   --    ["DIGITS"]
   --  where DIGITS is a sequence of 2, 4, 6 or 8 hexadecimal characters,
   --  return the corresponding character. Raise a
   --  Libadalang.Lexer.Invalid_Input if Pattern is invalid.

   function Canonicalize (Name : Text_Type) return Symbolization_Result
      with Inline;
   --  Return a canonicalized name for Name. This performs case folding and
   --  brackets decoding.

   -------------------------------
   -- Numeric literals handling --
   -------------------------------

   function Decode_Integer_Literal
     (Text : Text_Type) return GNATCOLL.GMP.Integers.Big_Integer;
   --  Turn Text, a valid Ada integer literal, into the signified integer
   --  (arbitrary precision). Raise a Libadalang.Analysis.Property_Error if
   --  Text is not a valid literal.

end Libadalang.Sources;
