package body Foo is
   procedure Test (X : in out U) is
   begin
      null;
   end Test;
   --% node.p_previous_part_for_decl()

   procedure Test (X : in out T) is
   begin
      null;
   end Test;
end Foo;

