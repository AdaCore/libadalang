procedure P
  (N : Integer;
   M : in Integer;
   O : in out Integer;
   P : out Integer)
   --% [x.p_is_constant_object for x in node.f_params]
is
   C : constant Integer := 0;
   --% node.p_is_constant_object
   D : Integer := 0;
   --% node.p_is_constant_object

   type A is array (Natural range <>) of Integer;
   E : constant A (1 .. 4) := (1, 2, 3, 4);
   F : A (1 .. 3) := (1, 2, 3);
   G, H : A (1 .. 2);

   Q, R : Integer;
begin
   D := N;
   --% node.f_dest.p_referenced_decl().p_is_constant_object
   --% node.f_expr.p_referenced_decl().p_is_constant_object
   O := D;
   --% node.f_dest.p_referenced_decl().p_is_constant_object
   P := M + C + D + O;
   --% node.f_dest.p_referenced_decl().p_is_constant_object

   Q := E(1);
   --% node.f_expr.p_is_constant
   R := F(1);
   --% node.f_expr.p_is_constant


   G := E(1 .. 2);
   --% node.f_expr.p_is_constant
   H := F(2 .. 3);
   --% node.f_expr.p_is_constant
end P;
