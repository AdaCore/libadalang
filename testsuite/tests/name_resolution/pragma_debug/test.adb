procedure Test is
   procedure Foo is null;
   function Foo return Integer is (1);
begin
   pragma Debug ((if True then True else False), Foo);
   pragma Test_Statement;
end Test;
