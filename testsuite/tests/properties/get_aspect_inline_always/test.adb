procedure Test is
   package S is
      procedure Foo;
      --% node.p_get_aspect('Inline_Always')
      procedure Foo (A : Integer) is null;
      --% node.p_get_aspect('Inline_Always')
      procedure Foo (S : String);
      --% node.p_get_aspect('Inline_Always')

      pragma Inline_Always (Foo);
   end S;

   package body S is
      procedure Foo is null;
      procedure Foo (S : String) is null;
   end S;
begin
   null;
end Test;
