procedure A is
   generic
      type Rec (D1 : Positive) is private;
   package Gen is
      X : Rec (D1 => 5);
   end Gen;

   type My_Rec_Type (D2 : Positive) is record
      D1 : Integer;
   end record;

   package P is new Gen (My_Rec_Type);
   --% x = node.p_designated_generic_decl.find(lal.ObjectDecl)
   --% d1 = x.f_type_expr.f_constraint.f_constraints[0].f_ids[0]
   --% d1.p_referenced_decl()

   X : Integer;
begin
   null;
end A;
