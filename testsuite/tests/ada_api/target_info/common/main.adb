with Ada.Containers.Ordered_Sets;
with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Analysis;           use Libadalang.Analysis;
with Libadalang.Target_Info;        use Libadalang.Target_Info;
with Libadalang.Target_Info_Getter; use Libadalang.Target_Info_Getter;

procedure Main is

   Prefix           : constant String := "target_info-";
   Suffix           : constant String := ".txt";
   Native           : Target_Information;
   Target_Displayed : Boolean := False;

   procedure Check (Label : String; ATP_File : String := "");
   --  Load the "p.gpr" project with the native runtime and the "ATP_FILE"
   --  external variable set (if provided), then create a context from it, and
   --  finally dump the target information associated to this context.

   package String_Sets is new Ada.Containers.Ordered_Sets (Unbounded_String);

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
   --  This main handles two kinds of tests:
   --
   --  * When the file "target_name.txt" exists, the test is meant to check
   --    that the ``Create_Context_From_Project`` function correctly sets the
   --    target info for the project target (for the set of known native
   --    targets). Each such test runs exclusively on a given native target,
   --    whose name is stored in the "target_name.txt" text file.
   --
   --  * Otherwise, the test is meant to check that Libadalang returns the
   --    right target information for all known native targets. It is redundant
   --    with the first kind of tests, but is much more convenient for
   --    development, as it runs on all native targets (it does not rely on
   --    GPR2 code to determine what the native target is).

   if Exists ("target_name.txt") then

      --  Fetch the expected target info for the native target

      declare
         F : File_Type;
         S : String (1 .. 80);
         L : Natural;
      begin
         Open (F, In_File, "target_name.txt");
         Get_Line (F, S, L);
         Close (F);

         while S (L) in ' ' | ASCII.CR | ASCII.LF loop
            L := L - 1;
         end loop;
         Native := Load (Prefix & S (1 .. L) & Suffix);
      end;

      Check ("Regular");
      Check ("Override ATP_FILE", "dummy.txt");

   else
      --  Try to load all tests

      declare
         Targets : String_Sets.Set;
         S       : Search_Type;
         E       : Directory_Entry_Type;
      begin
         --  Find the list of builtin targets: filenames matching
         --  "target_info-*.txt".

         Start_Search (S, ".", "");
         while More_Entries (S) loop
            Get_Next_Entry (S, E);
            declare
               N : constant String := Simple_Name (E);
            begin
               if N'Length > Prefix'Length + Suffix'Length
                  and then N (N'First .. N'First + Prefix'Length - 1) = Prefix
                  and then N (N'Last - Suffix'Length + 1 .. N'Last) = Suffix
               then
                  Targets.Insert
                    (To_Unbounded_String
                       (N (N'First + Prefix'Length
                           .. N'Last - Suffix'Length)));
               end if;
            end;
         end loop;
         End_Search (S);

         --  Dump the target info we have for each known native target

         for Target_Name of Targets loop
            declare
               T : constant String := To_String (Target_Name);
            begin
               Put_Line ("== " & T & " ==");
               New_Line;
               Dump (Get (T));
               New_Line;
            end;
         end loop;
      end;
   end if;
   Put_Line ("Done.");
end Main;
