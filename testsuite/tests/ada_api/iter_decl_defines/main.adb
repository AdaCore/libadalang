with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_File ("foo.adb");
   N   : constant Ada_Node :=
     Find_First
       (Root      => U.Root,
        Predicate => Decl_Defines ("Char_Set_Type"));
begin
   Put_Line ("Found: " & N.Image);
   Put_Line ("Done.");
end Main;
