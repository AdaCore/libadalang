package Invalid is

   --  Logical operators on non-modular integers are not supported

   Int_And : constant Integer := 5 and 3;
   --% node.f_default_expr.p_eval_as_int

   --  "not" on a non-modular integer is not supported

   Int_Not : constant Integer := not 5;
   --% node.f_default_expr.p_eval_as_int

end Invalid;
