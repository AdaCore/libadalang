procedure View is
   package P is
      type T is tagged null record;
      type T2 is new T with record
         A, B : Integer;
      end record;
   end P;

   V : P.T'Class := P.T2'(1, 2);
   V2 : constant P.T'Class := V;
   V3 : P.T'Class := V;

   X : Integer;
begin
   P.T2 (V3).A := P.T2 (V2).A;
   --% node.f_dest.p_is_constant
   --% node.f_expr.p_is_constant

   P.T2 (V).A := P.T2 (V3).A;
   --% node.f_dest.p_is_constant
   --% node.f_expr.p_is_constant

   P.T2 (View.V3).A := P.T2 (View.V2).A;
   --% node.f_dest.p_is_constant
   --% node.f_expr.p_is_constant

   P.T2 (View.V).A := P.T2 (View.V3).A;
   --% node.f_dest.p_is_constant
   --% node.f_expr.p_is_constant

   X := Integer (1.6);
   --% node.f_dest.p_is_constant
   --% node.f_expr.p_is_constant
end View;
