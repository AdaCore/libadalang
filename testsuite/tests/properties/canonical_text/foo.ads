procedure Foo is
   ["03C0"]
   --% node.p_canonical_text
   : constant := 3.14;
   This_Is_["03C0"]
   --% node.p_canonical_text
   : constant := 3.14;
   ["000003C0"]_Name
   --% node.p_canonical_text
   : constant String := "["0003C0"]";

   type C_Type is (
      'A'
      --% node.p_canonical_text
      ,
      'a'
      --% node.p_canonical_text
   );

   O_1 : constant Integer := 1 + 1;
   --% node.f_default_expr.f_op.p_canonical_text

   O_2 : constant Boolean := True and then False;
   --% node.f_default_expr.f_op.p_canonical_text

   O_3 : constant Boolean := 1 not in 1 | 2;
   --% node.f_default_expr.f_op.p_canonical_text
begin
   null;
end Foo;
