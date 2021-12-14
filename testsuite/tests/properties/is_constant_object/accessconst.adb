procedure AccessConst is
   type P_ACCESS_CONSTANT is access constant Integer;
   Var : P_ACCESS_CONSTANT := new Integer'(1);
   --% node.p_is_constant_object

   Const_Var_All : Integer renames Var.all;
   --% node.p_is_constant_object

   type P_ACCESS is access Integer;
   Vbr : P_ACCESS := new Integer'(1);
   --% node.p_is_constant_object

   Vbr_All : Integer renames Vbr.all;
   --% node.p_is_constant_object
begin
   null;
end AccessConst;
