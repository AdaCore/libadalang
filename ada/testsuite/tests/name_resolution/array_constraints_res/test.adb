procedure Test is
   function Foo return Integer is (1289);
   function Foo return Float is (158985.0);
   type A is array (1 .. Foo) of Integer;
begin
   null;
end Test;
pragma Test_Block;
