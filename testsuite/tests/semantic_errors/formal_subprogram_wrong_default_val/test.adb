procedure Foo is
   function Default_Add (A, B : Integer) return Integer;

   generic
      with function Add (A, B : Integer) return Integer
      is DefaAdd;
      pragma Test_Block (Expect_Fail => True);
   package Boo is
   end Boo;
begin
   null;
end Foo;
