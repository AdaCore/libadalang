procedure Test is
   type I is limited interface;

   task type T is new I with
      entry E;
   end T;
   pragma Test_Statement;
begin
   null;
end Test;
