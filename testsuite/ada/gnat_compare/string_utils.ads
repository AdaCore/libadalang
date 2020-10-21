with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.Strings; use GNAT.Strings;

with Libadalang.Helpers;

package String_Utils is

   function "+" (S : String) return Unbounded_String
                    renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String renames To_String;

   function Starts_With (S, Prefix : String) return Boolean is
     (S'Length >= Prefix'Length
      and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   function Strip_Prefix (S, Prefix : String) return String is
     (S (S'First + Prefix'Length .. S'Last));

   type Slice is record
      First : Positive;
      Last  : Natural;
   end record;
   --  Bounds for a slice of String

   function Get (Str : String; Sl : Slice) return String is
     (Str (Sl.First .. Sl.Last));
   --  Return the substring corresponding to the Sl slice in Str

   type Slice_Array is array (Positive range <>) of Slice;

   function Split
     (S          : String;
      Separators : String := " " & ASCII.HT & ASCII.LF & ASCII.CR)
      return Slice_Array
     with Post => Split'Result'First = 1;
   --  Split the S string for each occurence of any character in Separators and
   --  return the corresponding slices.

   function Read_Natural (S : String; Cursor : in out Positive) return Natural;
   --  Read as many digits as possible (at least one) starting from S (Cursor)
   --  and return the corresponding decoded number. Update Cursor to point past
   --  this sequence of digits.

   function Read_Character
     (S : String; Cursor : in out Positive) return Character;
   --  Return S (Cursor) and increment Cursor

   procedure Skip_Until (S : String; Cursor : in out Positive; C : Character);
   --  Look for an occurence of C in S starting at Cursor, update Cursor to
   --  point past that occurence.

   function Char_In_Set (C : Character; Set : String) return Boolean is
     ((for some SC of Set => SC = C));
   --  Determine whether C appears in Set

   function To_String_List
     (Strings : Libadalang.Helpers.String_Vectors.Vector) return String_List;

end String_Utils;
