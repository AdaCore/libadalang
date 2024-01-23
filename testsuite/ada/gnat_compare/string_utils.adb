with Ada.Containers.Vectors;

package body String_Utils is

   -----------
   -- Split --
   -----------

   function Split
     (S          : String;
      Separators : String := " " & ASCII.HT & ASCII.LF & ASCII.CR)
      return Slice_Array
   is
      package Slice_Vectors is new Ada.Containers.Vectors
        (Positive, Slice);
      Result : Slice_Vectors.Vector;
      First  : Natural := 0;
   begin
      for I in S'Range loop
         if Char_In_Set (S (I), Separators) then
            if First > 0 then
               Result.Append (Slice'(First, I - 1));
               First := 0;
            end if;
         elsif First = 0 then
            First := I;
         end if;
      end loop;
      if First > 0 then
         Result.Append (Slice'(First, S'Last));
      end if;

      declare
         Slices : Slice_Array (1 .. Result.Last_Index);
      begin
         for I in Slices'Range loop
            Slices (I) := Result.Element (I);
         end loop;
         return Slices;
      end;
   end Split;

   ------------------
   -- Read_Natural --
   ------------------

   function Read_Natural (S : String; Cursor : in out Positive) return Natural
   is
      Result : Natural := 0;
   begin
      if S (Cursor) not in '0' .. '9' then
         raise Constraint_Error;
      end if;

      while Cursor <= S'Last loop
         if S (Cursor) not in '0' .. '9' then
            return Result;
         end if;
         Result :=
           10 * Result + Character'Pos (S (Cursor)) - Character'Pos ('0');
         Cursor := Cursor + 1;
      end loop;
      return Result;
   end Read_Natural;

   --------------------
   -- Read_Character --
   --------------------

   function Read_Character
     (S : String; Cursor : in out Positive) return Character
   is
      Result : constant Character := S (Cursor);
   begin
      Cursor := Cursor + 1;
      return Result;
   end Read_Character;

   ----------------
   -- Skip_Until --
   ----------------

   procedure Skip_Until (S : String; Cursor : in out Positive; C : Character)
   is
   begin
      while S (Cursor) /= C loop
         Cursor := Cursor + 1;
      end loop;
   end Skip_Until;

   --------------------
   -- To_String_List --
   --------------------

   function To_String_List
     (Strings : Libadalang.Helpers.String_Vectors.Vector) return String_List
   is
      Result : String_List (1 .. Strings.Last_Index);
   begin
      for I in Result'Range loop
         Result (I) := new String'(+Strings.Element (I));
      end loop;
      return Result;
   end To_String_List;

end String_Utils;
