procedure Test is
   procedure Foo is null;
   procedure Bar is null;
   procedure Baz is null;

   pragma Inline (Foo, Bar, Baz);
   pragma Test_Statement;
begin
   null;
end Test;
