procedure Test is
   subtype My_Nat is Integer range 0 .. 100;

   type Arr is array (Integer range <>) of Integer;

   subtype Sub_Arr is Arr;
   subtype Nat_Arr is Sub_Arr (Natural);
   subtype Sub_Nat_Arr is Nat_Arr;

   type New_Nat_Arr is new Sub_Arr (Natural);

   type New_Arr is new Sub_Arr;

   subtype Nat_New_Arr is New_Arr (Natural);

   type Mat is array (Integer range <>, Integer range <>) of Integer;

   subtype Nat_Mat is Mat (Natural, Natural);

   type Vec_3 is array (1 .. 3) of Integer;

   type Discr_Rec (X, Y : Natural) is record
      K : Integer;
   end record;

   subtype Sub_Rec is Discr_Rec (X => 1, Y => 2);

   type Der_Rec is new Discr_Rec (X => 1, Y => 3);

   A : My_Nat;
   --% node.f_type_expr.p_subtype_constraint()

   B : Sub_Nat_Arr;
   --% node.f_type_expr.p_subtype_constraint()

   C : New_Nat_Arr;
   --% node.f_type_expr.p_subtype_constraint()

   D : Nat_New_Arr;
   --% node.f_type_expr.p_subtype_constraint()

   E : Nat_Mat;
   --% node.f_type_expr.p_subtype_constraint()

   F : Vec_3;
   --% node.f_type_expr.p_subtype_constraint()

   G : Sub_Rec;
   --% node.f_type_expr.p_subtype_constraint()

   H : My_Nat range 1 .. 10;
   --% node.f_type_expr.p_subtype_constraint()

   I : Sub_Arr (1 .. 3);
   --% node.f_type_expr.p_subtype_constraint()

   J : Discr_Rec (X => 1, Y => 4);
   --% node.f_type_expr.p_subtype_constraint()
begin
   null;
end Test;
