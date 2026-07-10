procedure Test is
   type Small is range 1 .. 100 with Default_Value => 7;
   pragma Test_Block;

   type Arr is array (1 .. 10) of Small
     with Default_Component_Value => 3;
   pragma Test_Block;
begin
   null;
end Test;
