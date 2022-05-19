procedure Test is
   generic
   package Foo is
      X : Integer;
      --% node.p_fully_qualified_name

      generic
      package Bar is
         Y : Integer;
         --% node.p_fully_qualified_name

         package Baz is
            Z : Integer;
            --% node.p_fully_qualified_name
         end Baz;
      end Bar;

      package Foo_Bar_Inst is new Bar;

      generic
      procedure Lol (X : Integer);
      --% node.p_fully_qualified_name

      procedure Foo_Lol is new Lol;
   end Foo;

   package Test_Foo_Inst is new Foo;
   package Test_Bar_Inst is new Test_Foo_Inst.Bar;

   procedure Test_Lol is new Test_Foo_Inst.Lol;

   -- Testing instances

   S : Integer;
begin
   S := Test_Foo_Inst.X;
   --% node.f_expr.p_referenced_decl().p_fully_qualified_name

   S := Test_Bar_Inst.Y;
   --% node.f_expr.p_referenced_decl().p_fully_qualified_name

   S := Test_Foo_Inst.Foo_Bar_Inst.Y;
   --% node.f_expr.p_referenced_decl().p_fully_qualified_name

   S := Test_Bar_Inst.Baz.Z;
   --% node.f_expr.p_referenced_decl().p_fully_qualified_name

   S := Test_Foo_Inst.Foo_Bar_Inst.Baz.Z;
   --% node.f_expr.p_referenced_decl().p_fully_qualified_name

   Test_Lol (X => 42);
   --% x = node.f_call.f_suffix[0].f_designator
   --% x.p_referenced_decl().p_fully_qualified_name

   Test_Foo_Inst.Foo_Lol (X => 42);
   --% x = node.f_call.f_suffix[0].f_designator
   --% x.p_referenced_decl().p_fully_qualified_name
end Test;
