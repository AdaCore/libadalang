procedure Pouet is
   type Integer is range 1 .. 100000000000;

   type T is record
      A, B : Integer;
   end record;

   type T_Ptr is access all T;

   I : T_Ptr;
   J : access T;
begin
   I := new T;
   pragma Test_Statement;

   J := new T'(12, 15);
   pragma Test_Statement;
end Pouet;
