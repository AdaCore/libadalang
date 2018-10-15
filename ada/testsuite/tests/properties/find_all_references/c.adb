package body C is
   function Foo return Integer is
   begin
      return B.Make_Rec_1 (2).U;
   end Foo;
end C;