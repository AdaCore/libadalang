procedure Testrange is
   procedure Foo (I : Integer) is null;
   procedure Foo (F : Float) is null;

   type Oops is array (Integer range 1 .. 10) of Float;

   Arr : Oops;
begin
   for I in Integer'Range loop
      Foo (I);
   end loop;

   for J in Oops'Range loop
      Foo (J);
      Foo (Arr (J));
   end loop;

   for J in Arr'Range loop
      Foo (J);
      Foo (Arr (J));
   end loop;

end Testrange;
pragma Test_Block;
