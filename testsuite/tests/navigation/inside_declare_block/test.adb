procedure Test is
begin
   declare
      type T;

      type T is null record;
      --% node.p_previous_part_for_decl()

      procedure Foo;

      procedure Foo is null;
      --% node.p_previous_part_for_decl()
   begin
      null;
   end;
end Test;
