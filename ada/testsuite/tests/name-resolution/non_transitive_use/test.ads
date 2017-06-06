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
   use Boo;
   pragma Test (A);
end Far;
