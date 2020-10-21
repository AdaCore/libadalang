package Pkg is

   type Rec is record
      I : Integer;
   end record;

   R : constant Rec := (I => 1);
   I : constant Integer := R.I;
   --% node.f_default_expr.p_eval_as_int

end Pkg;
