procedure Array_Type is
   type Arr is array (1 .. 32) of Integer;
   type Arr_2 is array (1 .. 32) of Integer;

   X : Arr;
   Y : Arr_2;
begin
   X := Arr (Y);
   --% node.f_expr.p_kind
end Array_Type;
