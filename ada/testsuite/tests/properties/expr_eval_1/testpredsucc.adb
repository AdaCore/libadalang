procedure Testpredsucc is
   type A is range 1 .. 200;
   type B is range A'Succ (A'First) .. A'Pred (A'Last);
   --% node.find(lal.BinOp).f_left.p_eval_as_int
   --% node.find(lal.BinOp).f_right.p_eval_as_int

   type En is (C, D, E, F);

   Inst : constant En := En'Succ (C);
   --% node.f_default_expr.p_eval_as_int

   Inst : constant En := En'Succ (F);
   --% node.f_default_expr.p_eval_as_int

   Inst2 : constant En := C;
   --% node.f_default_expr.p_eval_as_int

begin
   null;
end Testpredsucc;
