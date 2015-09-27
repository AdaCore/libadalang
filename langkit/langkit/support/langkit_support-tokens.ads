with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;

with Interfaces; use Interfaces;

with Langkit_Support.Text; use Langkit_Support.Text;

package Langkit_Support.Tokens is

   type Relative_Position is (Before, Inside, After);
   --  Where some source location is with respect to another/a source location
   --  range.

   type Source_Location is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record;

   type Source_Location_Range is record
      Start_Line, End_Line     : Unsigned_32;
      Start_Column, End_Column : Unsigned_16;
   end record;

   type Token is record
      Id         : Unsigned_16;

      Text       : Text_Access;
      --  Text as found in original source file or null depending on the token
      --  kind (as decided in the lexer specification). For instance: null for
      --  keywords but actual text for identiifers.

      Sloc_Range : Source_Location_Range;
   end record;

   type Token_Access is access all Token;

   No_Source_Location       : constant Source_Location       := (0, 0);
   No_Source_Location_Range : constant Source_Location_Range := (0, 0, 0, 0);
   No_Token                 : constant Token :=
     (0, null, No_Source_Location_Range);

   function Start_Sloc (Sloc_Range : Source_Location_Range)
                        return Source_Location is
     ((Line => Sloc_Range.Start_Line, Column => Sloc_Range.Start_Column));

   function End_Sloc (Sloc_Range : Source_Location_Range)
                      return Source_Location is
     ((Line => Sloc_Range.End_Line, Column => Sloc_Range.End_Column));

   function Make_Range (Start_Sloc, End_Sloc : Source_Location)
                        return Source_Location_Range is
     ((Start_Line   => Start_Sloc.Line,
       End_Line     => End_Sloc.Line,
       Start_Column => Start_Sloc.Column,
       End_Column   => End_Sloc.Column));

   function Compare (Left, Right : Source_Location) return Relative_Position
     with Pre => (Left /= No_Source_Location
                  and then Right /= No_Source_Location);
   --  Tell where Right is with respect to Left (before, inside = same sloc,
   --  after).

   function Compare (Sloc_Range : Source_Location_Range;
                     Sloc       : Source_Location) return Relative_Position
     with Pre => (Sloc_Range /= No_Source_Location_Range
                  and then Sloc /= No_Source_Location);
   --  Tell where Sloc is with respect to Sloc_Range

   function Image (Sloc : Source_Location) return String is
     (Ada.Strings.Fixed.Trim (Unsigned_32'Image (Sloc.Line), Left) & ':'
      & Ada.Strings.Fixed.Trim (Unsigned_16'Image (Sloc.Column), Left));

   function Image (Sloc_Range : Source_Location_Range) return String is
     (Image (Start_Sloc (Sloc_Range)) & '-'
      & Image (End_Sloc (Sloc_Range)));

   function Image (T : Token) return String is
     (if T.Text = null
      then ""
      else Image (T.Text.all));

end Langkit_Support.Tokens;
