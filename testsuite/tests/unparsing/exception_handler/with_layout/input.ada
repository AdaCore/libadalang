procedure Foo
is
begin
   declare
      Bar : Baz;
   begin
      null;
      null;
   exception
      when Qux =>
         null;
      when
        Quxz  =>
         null;
      when Qux | Corge
      =>  null;
      when
        Eeeeeeeeee :
          Quuuux 
          | Cooorge
      =>
         null;
      when others =>
         null;
   end;
end Foo;
