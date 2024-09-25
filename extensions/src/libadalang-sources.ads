--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.GMP.Integers;
with GNATCOLL.GMP.Rational_Numbers;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;

with Libadalang.Common;

--  General helpers to deal with sources text decoding

package Libadalang.Sources is

   use Support.Text;

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

   function Decode_Character_Literal (Text : Text_Type) return Character_Type;
   --  Turn Text, a valid Ada character literal, into the signified character.
   --  Raise a Libadalang.Common.Property_Error if Text is not a valid
   --  literal.

   function Decode_String_Literal
     (Text : Text_Type;
      Kind : Common.Ada_Node_Kind_Type :=
          Common.Ada_String_Literal) return Text_Type;
   --  Turn Text, a valid Ada string literal, into the signified string (this
   --  function can also decode interpolated string chunks by setting Kind to
   --  Ada_Format_String_[End|Mid|Start|String]). Raise a
   --  Libadalang.Common.Property_Error if Text is not a valid literal.

   -------------------------------
   -- Numeric literals handling --
   -------------------------------

   procedure Decode_Integer_Literal
     (Text   : Text_Type;
      Result : out GNATCOLL.GMP.Integers.Big_Integer);
   --  Turn Text, a valid Ada integer literal, into the signified integer
   --  (arbitrary precision). Raise a Libadalang.Analysis.Property_Error if
   --  Text is not a valid literal.
   --
   --  TODO(T206-025): use an OUT parameter instead of a function return type
   --  to workaround a GNAT finalization bug when the function raises an
   --  exception.

   procedure Decode_Real_Literal
     (Text   : Text_Type;
      Result : out GNATCOLL.GMP.Rational_Numbers.Rational);
   --  Turn Text, a valid Ada real literal, into a rational number. Raise a
   --  Libadalang.Analysis.Property_Error if Text is not a valid literal.

   subtype Numerical_Base is Natural range 2 .. 16;

   function Encode_Integer_Literal
     (Value : GNATCOLL.GMP.Integers.Big_Integer;
      Base  : Numerical_Base := 10) return Text_Type;
   --  Return an Ada literal for the given integer

   function Encode_Real
     (Value : GNATCOLL.GMP.Rational_Numbers.Rational;
      Base  : Numerical_Base := 10) return Text_Type;
   --  Return an Ada static expression that evaluates to the given real

end Libadalang.Sources;
