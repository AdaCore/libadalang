procedure Test is
   procedure Foo (X : Integer);
   --% node.f_subp_spec.p_name
   procedure Foo (X : integer) is null;
   --% node.f_subp_spec.p_name

   function Bar (X : Integer) return Integer is (X);
   --% node.f_subp_spec.p_name

   task T is
      entry Baz (X : Integer);
      --% node.f_spec.p_name
   end T;

   task body T is
   begin
      accept Baz (X : Integer) do
         -- this is not a subprogram specification
         null;
      end Baz;
   end T;

   X : String := Integer'Image (2);
   --% node.f_default_expr.p_called_subp_spec.p_name

   Y : String := Integer'(2)'Image;
   --% node.f_default_expr.p_called_subp_spec.p_name
begin
   null;
end Test;
