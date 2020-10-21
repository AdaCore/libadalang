procedure Foo is
   π        : constant Float := 3.14;
   ["00E9"] : Wide_Character := '["00E9"]';

begin
   --  Test case folding: the above is a lower-case pi while there is an
   --  upper-case below.
   pragma Test (Π);

   --  Test brackets decoding
   pragma Test (É);
end Foo;
