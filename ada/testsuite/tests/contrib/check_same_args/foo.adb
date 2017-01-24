procedure Foo is
   Y     : Integer := I;
   Dummy_1 : constant Integer := Y / Y;
begin
   for I in Y .. Y loop
      null;
   end loop;
end Foo;
