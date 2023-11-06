with Ada.Text_IO; use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

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
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
      end if;
      U.Root.Print;
      New_Line;
   end Process;

   --  Create a context from the example project

   Tree : GPR2.Project.Tree.Object;
begin
   Tree.Load_Autoconf
     (Filename => GPR2.Path_Name.Create_File
                    ("p.gpr", GPR2.Path_Name.No_Resolution),
      Context  => GPR2.Context.Empty);
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
