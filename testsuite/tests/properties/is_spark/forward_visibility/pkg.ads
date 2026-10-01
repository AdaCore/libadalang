package Pkg is
   Early : constant Boolean := True;

   procedure Valid_Nameres_Aspect is null with SPARK_Mode => Early;
   --% node.f_aspects.f_aspect_assocs[0].f_expr.p_referenced_decl()

   procedure Valid_Nameres_Pragma is null;
   pragma SPARK_Mode (Early);
   --% node.f_args[0].f_expr.p_referenced_decl()

   procedure Invalid_Nameres_Aspect is null with SPARK_Mode => Late;
   --% node.f_aspects.f_aspect_assocs[0].f_expr.p_referenced_decl()

   procedure Invalid_Nameres_Pragma is null;
   pragma SPARK_Mode (Late);
   --% node.f_args[0].f_expr.p_referenced_decl()

   Late : constant Boolean := True;
end Pkg;
