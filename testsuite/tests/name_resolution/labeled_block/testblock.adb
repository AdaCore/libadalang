procedure Testblock is
begin
   Foo :declare
      A : Integer;
   begin
      A := A + 1;
      <<B>>
      goto Foo.B;
      pragma Test_Statement;
   end Foo;
end Testblock;
