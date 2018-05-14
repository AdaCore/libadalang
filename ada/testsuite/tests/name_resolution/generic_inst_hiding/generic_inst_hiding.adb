procedure Reprow is
   package A is
      generic
      package B is
      end B;
   end A;

   --  This is not actually compilable by GNAT, but it will compile if A is a
   --  library level package that is with-ed, so both need to at least resolve
   --  from LAL's pov.

   package A is new A.B;
   pragma Test_Statement;
begin
   null;
end Reprow;
