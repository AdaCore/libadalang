procedure Test_Range is

   type Color is (Red, Green, Blue);

   type A is array (Integer range <>, Color range <>) of Natural;

   Inst : A := (1 .. 10 => (Red .. Blue => 20));

   S : Natural;
begin
   for I in Inst'Range (1) loop
      for J in Inst'Range (2) loop
         S := Inst (I, J);
      end loop;
   end loop;

end Test_Range;
pragma Test_Block;
