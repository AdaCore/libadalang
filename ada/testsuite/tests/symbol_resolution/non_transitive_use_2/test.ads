package Test is
   A : Integer;
end Test;

pragma Section ("In Boo");

package Boo is
   use Test;
   pragma Test (A);
end Boo;

pragma Section ("In Far");

package Far is
   pragma Test (Boo.A);
end Far;
