--  Test the resolution of a name to the corresponding object declaration

package body Lol is
   package Koin is
      Fu : Integer;
   end Koin;

   procedure Bar is
      A, B : Integer;
   begin
      declare
         subtype Lol is Integer range 0 .. 10;
      begin
         A := B + 2 + Koin.Fu;
         pragma Test (Koin.Fu);
      end;
   end Bar;
end Lol;
