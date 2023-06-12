procedure Test is

   generic
      type T;
   package P is
   end P;

   package body P is
   end P;

   generic
      type T;
      with package Q is new P (T);
   package R is
   end R;

   package body R is
   end R;

   package I is new P (Integer);

   package J is new R (T => Integer, Q => I);
   pragma Test_Statement;

begin
   null;
end Test;
