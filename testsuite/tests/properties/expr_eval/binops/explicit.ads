package Explicit is

   Int_Add : constant := "+" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Add_2 : constant := Standard."+" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Sub : constant := "-" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Mul : constant := "*" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Div : constant := "/" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Pow : constant := "**" (2, 3);
   --% node.f_expr.p_eval_as_int

   Int_Eq : constant Boolean := "=" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Int_Ne : constant Boolean := "/=" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Int_Lt : constant Boolean := "<" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Int_Gt : constant Boolean := ">" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Int_Lte : constant Boolean := "<=" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Int_Gte : constant Boolean := ">=" (2, 3);
   --% node.f_default_expr.p_eval_as_int

   Bool_And : constant Boolean := "and" (True, False);
   --% node.f_default_expr.p_eval_as_int

   Bool_Or : constant Boolean := "or" (False, True);
   --% node.f_default_expr.p_eval_as_int

   Bool_Xor_1 : constant Boolean := "xor" (False, True);
   --% node.f_default_expr.p_eval_as_int

   Str_Concat : constant String := "&" ("a", "b");
   Above_Len : constant := Str_Concat'Length;
   --% node.f_expr.p_eval_as_int

end Explicit;
