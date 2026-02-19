package Invalid is

   type My_Fixed is delta 0.1 range 0.0 .. 10.0;

   Unsupported_Float : constant Integer := Float'Width;
   --% node.f_default_expr.p_eval_as_int

   Unsupported_Fixed : constant Integer := My_Fixed'Width;
   --% node.f_default_expr.p_eval_as_int

   type My_Record is record
      X : Integer;
   end record;

   Bad_Record : constant Integer := My_Record'Width;
   --% node.f_default_expr.p_eval_as_int

   Bad_Args : constant Integer := Integer'Width (0);
   --% node.f_default_expr.p_eval_as_int

end Invalid;
