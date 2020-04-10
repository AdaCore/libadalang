procedure Testrange is
   procedure Foo (I : Integer) is null;
   procedure Foo (F : Float) is null;

   type Oops is array (Integer range 1 .. 10) of Float;

   type Oops_Oops is array
     (Integer range 1 .. 10, Boolean) of Integer;

   One : constant := 1;
   Two : constant := 2;

   Arr : Oops;
   Arr_Arr : Oops_Oops;
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

   for J in Arr_Arr'Range (One) loop
      Foo (Arr_Arr (J, True));
   end loop;

   for J in Arr_Arr'Range (Two) loop
      Foo (Arr_Arr (1, J));
   end loop;

end Testrange;
pragma Test_Block;
