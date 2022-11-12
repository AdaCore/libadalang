procedure Test is

   task type T is
      entry E;
   end T;

   task body T is
      N : Natural;
   begin
      T.N := 1;
      pragma Test_Statement;
   end T;

begin
   null;
end Test;
