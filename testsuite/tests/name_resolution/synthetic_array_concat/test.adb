procedure Test is
   X : String (1 .. 2);
begin
   X := "&" ('a', 'b');
   pragma Test_Statement;

   X := "&" ('a', "b");
   pragma Test_Statement;

   X := "&" ("a", 'b');
   pragma Test_Statement;

   X := "&" ("a", "b");
   pragma Test_Statement;
end Test;
