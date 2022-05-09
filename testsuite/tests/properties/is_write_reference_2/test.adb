procedure Test is
   procedure Foo (X : out Natural) is
   begin
      X := 0;
   end Foo;

   procedure Bar (X : out Integer) is
   begin
      Foo (Integer (X));
      --% node[0][1][0][1]
      --% node[0][1][0][1].p_is_write_reference()
      --% node[0][1][0][1][1][0][1]
      --% node[0][1][0][1][1][0][1].p_is_write_reference()
   end Bar;
begin
   null;
end Test;
