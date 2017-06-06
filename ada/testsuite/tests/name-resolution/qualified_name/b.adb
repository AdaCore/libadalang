with A;

procedure B is
   I, I2 : Integer;
begin
   I := A.Foo;
   pragma Test_Statement;

   I2 := A.Foo2;
   pragma Test_Statement;
end B;
