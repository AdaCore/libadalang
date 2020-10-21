procedure What is
   procedure Foo is null;

   generic
      type T is private;
   package Boo is
      procedure Baa (Self : T) is null;
   end Boo;

   generic
      type T is private;
      with package Bla is new Boo (T);
      with procedure Whatever (Self : T) is Bla.Baa;
      pragma Test_Statement;
   package Wat is
   end Wat;
begin
   null;
end What;
