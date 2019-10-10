procedure Test is
   task T is
      entry Foo;
   end T;

   task body T is
   begin
      accept Foo;
   end T;
begin
   T.Foo;
   pragma Test_Statement;
end Test;
