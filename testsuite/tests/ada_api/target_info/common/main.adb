with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Analysis;    use Libadalang.Analysis;
with Libadalang.Target_Info; use Libadalang.Target_Info;

procedure Main is

   Native           : constant Target_Information := Load ("native.txt");
   Target_Displayed : Boolean := False;

   procedure Check (Label : String; ATP_File : String := "");
   --  Load the "p.gpr" project with the native runtime and the "ATP_FILE"
   --  external variable set (if provided), then create a context from it, and
   --  finally dump the target information associated to this context.

   -----------
   -- Check --
   -----------

   procedure Check (Label : String; ATP_File : String := "") is
      Ctx : Analysis_Context;
      TI  : Target_Information;
      O   : GPR2.Options.Object;
      T   : GPR2.Project.Tree.Object;
   begin
      O.Add_Switch (GPR2.Options.P, "p.gpr");
      if ATP_File /= "" then
         O.Add_Switch (GPR2.Options.X, "ATP_FILE=" & ATP_File);
      end if;

      if not T.Load
        (O,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error with "could not load the project file";
      end if;

      if not Target_Displayed then
         Put_Line
           ("Canonical target name: " & String (T.Target (Canonical => True)));
         New_Line;
         Target_Displayed := True;
      end if;

      Put_Line ("== " & Label & " ==");
      New_Line;

      Ctx := Create_Context_From_Project (T);
      TI := Ctx.Get_Target_Information;
      Dump (TI);

      if ATP_File = "" and then TI /= Native then
         raise Program_Error with "unexpected native target info";
      end if;
      New_Line;
   end Check;

begin
   Check ("Regular");
   Check ("Override ATP_FILE", "dummy.txt");
   Put_Line ("Done.");
end Main;
