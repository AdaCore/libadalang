package Valid is

   Int_Max_1 : constant Integer := Integer'Max (1, 2);
   --% node.f_default_expr.p_eval_as_int

   Int_Max_2 : constant Integer := Integer'Max (5, 4);
   --% node.f_default_expr.p_eval_as_int

   Int_Min_1 : constant Integer := Integer'Min (1, 2);
   --% node.f_default_expr.p_eval_as_int

   Int_Min_2 : constant Integer := Integer'Min (5, 4);
   --% node.f_default_expr.p_eval_as_int

end Valid;
