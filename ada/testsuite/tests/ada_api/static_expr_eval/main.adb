with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Expr_Eval; use Libadalang.Expr_Eval;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx  : Analysis_Context := Create;
   Unit : Analysis_Unit := Get_From_File (Ctx, "test.adb");

   function Is_Object_Decl (N : Ada_Node) return Boolean
   is (Kind (N) in Ada_Object_Decl);
begin
   for E of Find (Root (Unit), Is_Object_Decl'Access).Consume loop
      declare
         Res : Eval_Result := Expr_Eval (E.As_Object_Decl.F_Default_Expr);
      begin
         Put_Line ("Expr " & Short_Image (E) & " evaluated to " & Image (Res));
         if Res.Kind in Int | Enum_Lit then
            Put_Line ("   Int value is " & As_Int (Res).Image);
         end if;
         Put_Line ("");
      end;
   end loop;

   Destroy (Ctx);
   Put_Line ("Done.");
end Main;
