with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   Ctx : Analysis_Context;

   procedure Process (Filename : String);
   --  Parse the requested file and print its diagnostics and parse tree

   -------------
   -- Process --
   -------------

   procedure Process (Filename : String) is
      U : constant Analysis_Unit :=
        Ctx.Get_From_File (Filename, Charset => "utf-8");
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop

            --  Remove absolute paths from error messages to have consistent
            --  baselines.

            declare
               Msg    : constant String := U.Format_GNU_Diagnostic (D);
               Prefix : constant String :=
                 "no_such_file.adb: Cannot open ";
            begin
               if Msg'Length > Prefix'Length
                  and then Msg (Msg'First .. Msg'First + Prefix'Length - 1)
                           = Prefix
               then
                  Put_Line (Prefix & "[...]");
               else
                  Put_Line (Msg);
               end if;
            end;
         end loop;
         New_Line;
      end if;
      U.Root.Print;
      New_Line;
   end Process;

   --  Create a context from the example project

   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;
begin
   Options.Add_Switch (GPR2.Options.P, "p.gpr");
   if not Tree.Load
            (Options,
             With_Runtime         => True,
             Artifacts_Info_Level => GPR2.Sources_Units,
             Absent_Dir_Error     => GPR2.No_Error)
   then
      raise Program_Error;
   end if;
   Ctx := Create_Context_From_Project (Tree);

   --  As a sanity check, first make sure that preprocessing is active

   Process ("foo.adb");

   --  Then check error cases: the source file cannot be read

   Process ("no_such_file.adb");

   --  Preprocessing aborts because of a syntax error

   Process ("invalid.adb");

   --  Buffer decoding after preprocessing fails because of an encoding error

   Process ("bad_encoding.bin");

   Put_Line ("Done.");
end Main;
