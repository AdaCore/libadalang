------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

with GNATCOLL.GMP.Integers;

with Libadalang.Common; use Libadalang.Common;

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

   function Decode_String_Literal (Text : Text_Type) return Text_Type;
   --  Turn Text, a valid Ada tsring literal, into the signified string.  Raise
   --  a Libadalang.Common.Property_Error if Text is not a valid literal.

   -------------------------------
   -- Numeric literals handling --
   -------------------------------

   function Decode_Integer_Literal
     (Text : Text_Type) return GNATCOLL.GMP.Integers.Big_Integer;
   --  Turn Text, a valid Ada integer literal, into the signified integer
   --  (arbitrary precision). Raise a Libadalang.Analysis.Property_Error if
   --  Text is not a valid literal.

end Libadalang.Sources;
