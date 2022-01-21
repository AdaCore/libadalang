procedure Callp is
   function Function_With_All_Default_Params
     (p1 : in Integer := 0;
      p2 : in Integer := 0;
      p3 : in Integer := 0) return Integer is
   begin
      return p1 + p2 + p3;
   end Function_With_All_Default_Params;

   function Function_With_All_Default_Params2
     (p1, p2 : in Integer := 0;
      p3 : in Integer := 0) return Integer is
   begin
      return p1 + p2 + p3;
   end Function_With_All_Default_Params2;

   function Function_With_All_Default_Params3
     (p1 : in Integer := 0;
      p2, p3 : in Integer := 0) return Integer is
   begin
      return p1 + p2 + p3;
   end Function_With_All_Default_Params3;

   function Function_With_All_Default_Params4
     (p1 : in Integer := 0;
      p2, p3 : in Integer := 0) return Integer
   is (p1 + p2 + p3);

   function Function_With_Default_Params
     (p1 : Integer; p2, p3 : in Integer := 0) return Integer
   is (p1 + p2 + p3);

   X : Integer;
begin
   X := Function_With_All_Default_Params;
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(0,1,2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(p1 => 0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(0, p3 => 0, p2 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params(p2 => 0, p3 => 1);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2;
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(0,1,2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(p1 => 0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(0, p3 => 0, p2 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params2(p2 => 0, p3 => 1);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3;
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(0,1,2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(p1 => 0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(0, p3 => 0, p2 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params3(p2 => 0, p3 => 1);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4;
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(0,1,2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(p1 => 0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(0, p2 => 1, p3 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(0, p3 => 0, p2 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_All_Default_Params4(p2 => 0, p3 => 1);
   --% node.f_expr.p_call_params

   X := Function_With_Default_Params(0, p3 => 0, p2 => 2);
   --% node.f_expr.p_call_params

   X := Function_With_Default_Params(0, p3 => 1);
   --% node.f_expr.p_call_params
end Callp;
