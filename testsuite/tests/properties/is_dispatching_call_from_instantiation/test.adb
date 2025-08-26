procedure Test is
   generic
      with procedure Compute_One_Value (U : Integer);
   procedure Compute_For_Num_Or_Vectors;

   procedure Compute_For_Num_Or_Vectors is
   begin
      Compute_One_Value (U => 1);
      --% callexpr = node.f_call
      --% callexpr.p_is_dispatching_call()
   end;

   procedure Get_Input_Aux (I : Integer) is null;

   procedure Get_Input_Aux_For_Num_Or_Vectors is new
     Compute_For_Num_Or_Vectors (Get_Input_Aux);
   --% gen_body = node.p_designated_generic_decl.p_body_part()
   --% callexpr = gen_body.find(lal.CallExpr)
   --% callexpr.p_is_dispatching_call()
begin
   null;
end Test;


