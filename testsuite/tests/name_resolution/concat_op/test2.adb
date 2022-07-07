with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Test2 is
   S : String := "Hello" & '.' & "." & '.' & "World!";
   pragma Test_Statement;

   U : String := "Good" & ('/' & '\') & " bye";
   pragma Test_Statement;

   type T is array (Positive range <>) of Integer;

   X : T := (1, 2, 3);
   pragma Test_Statement;
   Y : T := X & (4, 5, 6);
   pragma Test_Statement;
   Z : T := 7 & Y & 8;
   pragma Test_Statement;

   US1 : Unbounded_String := To_Unbounded_String ("!!!");
   US2 : Unbounded_String;
begin
   US2 := 'U' & "nbounded" & US1;
   pragma Test_Statement_UID;
   US2 := US1 & "dednuobn" & 'U';
   pragma Test_Statement_UID;
end Test2;
