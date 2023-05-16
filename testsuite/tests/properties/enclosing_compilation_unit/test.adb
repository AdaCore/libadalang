package body Test is
   procedure Foo is
      X : Integer;
   begin
      X := 2;
      --% node.p_enclosing_compilation_unit
   end Foo;
end Test;
