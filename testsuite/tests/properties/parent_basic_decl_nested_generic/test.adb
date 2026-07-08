procedure Test is
   generic
   package A is
      generic
      package G_Nested is
         type T is null record;
      end G_Nested;
   end A;

   package B is new A;

   generic
   package C is
      package Inst is new B.G_Nested;
   end C;

   package D is new C;

   V : D.Inst.T;
   --% t = node.f_type_expr.p_designated_type_decl
   --% p1 = t.p_parent_basic_decl
   --% p2 = p1.p_parent_basic_decl
   --% p3 = p2.p_parent_basic_decl
   --% p4 = p3.p_parent_basic_decl
   --% p5 = p4.p_parent_basic_decl
begin
   null;
end Test;
