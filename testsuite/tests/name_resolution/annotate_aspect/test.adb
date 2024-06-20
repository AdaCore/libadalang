procedure Test is
   function Foo (X : Integer) return Integer is (X);
   pragma Annotate
     (My_Tool,
      My_Command,
      "foo",                                      --  string literals
      "foo" & "bar",                              --  string concatenations
      "foo" & Wide_Character'Val (16#2665#),      --  or wide strings
      "foo" & Wide_Wide_Character'Val (16#2665#), --  or wide wide strings
      2 + 2,                                      --  arbitrary expression
      Entity => Foo);                             --  the associated entity
   pragma Test_Statement;

   function Bar (X : Integer) return Integer is (X)
      with Annotate =>
        (My_Tool,
         My_Command,
         "foo",
         "foo" & "bar",
         "foo" & Wide_Character'Val (16#2665#),
         "foo" & Wide_Wide_Character'Val (16#2665#),
         2 + 2);
   pragma Test_Block;
begin
   null;
end Test;
