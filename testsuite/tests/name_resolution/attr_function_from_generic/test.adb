procedure Test is
   generic
      type T is (<>);
   package Fail_G is
      function F (E : T) return T is (T'Val (1));
   end Fail_G;

   generic
   package Level_1 is
      generic
         type Type_T is (<>);
      package Level_2 is
         package Inst is new Fail_G (Type_T);
      end Level_2;
   end Level_1;

   package Origin is new Level_1;
   pragma Test_Statement;
begin
   null;
end Test;

