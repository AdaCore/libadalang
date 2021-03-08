procedure Test is
   function Temp (x,y: Integer) return Boolean is Null;

   procedure Outer_Procedure (x: Integer) is
      function Inner_Procedure (x: Integer) return Boolean is
         begin
           return Temp (Outer_Procedure.x, x);
           --% node.f_return_expr.f_suffix[0].f_r_expr.f_prefix.p_is_call
         end Inner_Procedure;
   begin
      Null;
   end Outer_Procedure;
begin
   null;
end Test;
