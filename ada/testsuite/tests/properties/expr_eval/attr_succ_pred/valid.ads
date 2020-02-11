package Valid is

   Int_Succ : constant Integer := Integer'Succ (1);
   --% node.f_default_expr.p_eval_as_int

   Int_Pred : constant Integer := Integer'Pred (1);
   --% node.f_default_expr.p_eval_as_int

   type Enum_Type is (A, B, C);

   Enum_Succ_1 : constant Enum_Type := Enum_Type'Succ (A);
   --% node.f_default_expr.p_eval_as_int

   Enum_Succ_2 : constant Enum_Type := Enum_Type'Succ (B);
   --% node.f_default_expr.p_eval_as_int

   Enum_Pred_1 : constant Enum_Type := Enum_Type'Pred (B);
   --% node.f_default_expr.p_eval_as_int

   Enum_Pred_2 : constant Enum_Type := Enum_Type'Pred (C);
   --% node.f_default_expr.p_eval_as_int

end Valid;
