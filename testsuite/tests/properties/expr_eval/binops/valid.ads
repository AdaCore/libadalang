package Valid is

   -----------------------------------
   -- Binary operations on integers --
   -----------------------------------

   Int_Add : constant := 2 + 3;
   --% node.f_expr.p_eval_as_int

   Int_Sub : constant := 2 - 3;
   --% node.f_expr.p_eval_as_int

   Int_Mul : constant := 2 * 3;
   --% node.f_expr.p_eval_as_int

   Int_Div : constant := 2 / 3;
   --% node.f_expr.p_eval_as_int

   Int_Pow : constant := 2 ** 3;
   --% node.f_expr.p_eval_as_int

   --  TODO: add tests for binops on reals once P_Eval_As_Real is exposed in
   --  the public API.

   --------------------------------
   -- Binary operations on bools --
   --------------------------------

   Bool_And_Then_1 : constant Boolean := True and then True;
   --% node.f_default_expr.p_eval_as_int

   Bool_And_Then_2 : constant Boolean := True and then False;
   --% node.f_default_expr.p_eval_as_int

   Bool_And_Then_3 : constant Boolean := False and then False;
   --% node.f_default_expr.p_eval_as_int

   Bool_And_Then_4 : constant Boolean := False and then True;
   --% node.f_default_expr.p_eval_as_int

   Bool_And_Then_5 : constant Boolean := True and False;
   --% node.f_default_expr.p_eval_as_int

   Bool_Or_Else_1 : constant Boolean := True or else True;
   --% node.f_default_expr.p_eval_as_int

   Bool_Or_Else_2 : constant Boolean := True or else False;
   --% node.f_default_expr.p_eval_as_int

   Bool_Or_Else_3 : constant Boolean := False or else False;
   --% node.f_default_expr.p_eval_as_int

   Bool_Or_Else_4 : constant Boolean := False or else True;
   --% node.f_default_expr.p_eval_as_int

   Bool_Or_Else_5 : constant Boolean := False or True;
   --% node.f_default_expr.p_eval_as_int

end Valid;
