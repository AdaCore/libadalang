package Ghost is
   X : Integer with Ghost => L3;
   pragma Test_Block;

   Y : Integer;
   pragma Ghost (L3);
   pragma Test_Statement;
end Ghost;
