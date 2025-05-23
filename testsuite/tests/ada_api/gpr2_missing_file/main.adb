with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

with Support;

procedure Main is
   procedure Check (Project_File : String);

   -----------
   -- Check --
   -----------

   procedure Check (Project_File : String) is
      Opts : GPR2.Options.Object;
      Tree : GPR2.Project.Tree.Object;
      Ctx  : Analysis_Context;
      U    : Analysis_Unit;
      D    : Basic_Decl;
      B    : Body_Node;
   begin
      Put_Line ("# " & Project_File);
      New_Line;
      Opts.Add_Switch (GPR2.Options.P, Project_File);
      if not Tree.Load
        (Options              => Opts,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error;
      end if;

      Ctx :=
        Create_Context
          (Unit_Provider => Create_Project_Unit_Provider (Tree),
           Event_Handler => Support.Create_Event_Handler);
      U := Ctx.Get_From_File ("pkg.1.ada");
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      D :=
        U.Root
        .As_Compilation_Unit.F_Body
        .As_Library_Item.F_Item
        .As_Package_Decl.F_Public_Part
        .F_Decls
        .Child (1)
        .As_Basic_Decl;
      Put_Line ("Fetching the body of " & D.Image);
      B := D.P_Body_Part_For_Decl;
      Put_Line ("  -> " & B.Image);

      New_Line;
   end Check;

begin
   Check ("p.gpr");
   Check ("abstract_prj.gpr");

   Put_Line ("main.adb: Done");
end Main;
