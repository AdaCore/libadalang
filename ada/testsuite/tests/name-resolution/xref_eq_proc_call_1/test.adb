procedure Lol is
   type Int is range 1 .. 110;
   procedure A (I : Int);

   B : Int;
begin
   A (B);
   pragma Test_Statement;
end Lol;
