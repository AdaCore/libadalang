procedure Lol is
   type Int is range 1 .. 110;
   procedure A (I : Int);
   function C (I : Int) return Int;

   B : Int;
begin
   A (12);
   pragma Test_Statement;

   B := C (12);
   pragma Test_Statement;

   C (12);
   pragma Test_Statement;
end Lol;
