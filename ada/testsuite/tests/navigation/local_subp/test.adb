procedure Test is
begin
   declare
      procedure Foo;
      --% node.p_defining_name.p_next_part()

      procedure Foo is null;
   begin
      Foo;
   end;
end Test;
