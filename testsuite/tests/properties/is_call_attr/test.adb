procedure Test is
   I : Integer := 42;

   X : String := Integer'Image (I);
   --% node.f_default_expr.p_is_call

   Y : String := I'Image;
   --% node.f_default_expr.p_is_call
   --% node.f_default_expr.p_call_params
begin
   null;
end Test;
