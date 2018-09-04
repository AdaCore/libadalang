procedure Testinline is
   procedure Foo is
      pragma Inline (Foo);
   begin
      null;
   end Foo;
begin
   null;
end Testinline;
