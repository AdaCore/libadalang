procedure Main is
   type My_Int is range 1 .. 20;
   type My_Float is digits 8;
   E : My_Int;
   procedure Kikol (A : My_Int; B, C : My_Float := 12.0; D : My_Int);
begin
   Kikol (A => E, D => E);
   pragma Test_Statement;
end;
