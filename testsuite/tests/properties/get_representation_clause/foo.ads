package Foo is
   subtype U16 is Integer range 0 .. 2 * 16 - 1;
   type Arr is array (1 .. 16) of U16;
   for Arr'Size use 15 * 32;
   for Arr'Alignment use Standard'Maximum_Alignment;
end Foo;
