procedure Test2 is
   generic
     type T is private;
     with function F return T is <>;
   procedure PG;

   procedure PG is
     O : constant T := PG.F;
     pragma Test_Statement;
   begin
     null;
   end PG;
begin
   null;
end Test2;
