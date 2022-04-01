procedure Test is
   generic
      type T is (<>);
   package P is
      subtype Sub_T is T;
      Pac_Var : T;
   end P;

   package P3 is new P (Character);
   use P3;
begin
   Pac_Var := Sub_T'('A');
   --% node.f_expr.f_suffix.p_eval_as_int
end Test;

