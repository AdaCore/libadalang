package Invalid is

   function F return Integer is (0);
   type Rec is null record;

   Bad_Decl : constant Integer := F'First;
   --% node.f_default_expr.p_eval_as_int

   Bad_Type : constant Integer := Rec'First;
   --% node.f_default_expr.p_eval_as_int

   Unknown_Decl : constant Integer := Invalid_Subtype'First;
   --% node.f_default_expr.p_eval_as_int

end Invalid;
