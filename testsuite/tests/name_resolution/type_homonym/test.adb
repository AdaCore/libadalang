procedure Test is
   type Foo is record
      Foo : Integer;
   end record;

   function Bar (I : Integer) return Foo is (Foo => I);

   function Bar (I : Integer) return String is
      X : Integer := Bar (12).Foo;
      pragma Test_Statement;
   begin
      return "";
   end Bar;
begin
   null;
end Test;
