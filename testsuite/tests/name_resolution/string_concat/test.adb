procedure Test is
   A : constant Character := 'l';
   B : constant Character := ASCII.NUL;

   E : constant String := "oiy";
   C : constant String := 'c' & "lol";
   D : constant String := C & "lol";
   F : constant String := A & D;
   G : constant String := "lol" & B;

begin
   null;
end Test;
pragma Test_Block;
