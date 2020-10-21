procedure Foo is
   I : constant Integer := 1;

   S : constant String := I'Img;
   pragma Test_Statement;

begin
   null;
end Foo;
