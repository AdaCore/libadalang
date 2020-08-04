procedure Test is
   package Name is
      type Named is limited interface;

      function Get_Name (X : Named) return String is abstract;
   end Name;

   package Foo is
      use Name;

      type Has_Foo is limited interface and Named;
   end Foo;

   use Foo;

   function Bar (X : Has_Foo'Class) return String is
   begin
      return Get_Name (X);
      pragma Test_Statement;
   end Bar;
begin
   null;
end Test;
