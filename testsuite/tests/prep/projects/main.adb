with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Langkit_Support.Errors;   use Langkit_Support.Errors;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   type Project_Names is array (Positive range <>) of Unbounded_String;

   procedure Process
     (Filename : String;
      Projects : Project_Names := (1 => Null_Unbounded_String));

   -------------
   -- Process --
   -------------

   procedure Process
     (Filename : String;
      Projects : Project_Names := (1 => Null_Unbounded_String))
   is
      Env     : Project_Environment_Access;
      Tree    : Project_Tree;
      Project : Project_Type := No_Project;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      Initialize (Env);
      Tree.Load (Create (+Filename));

      for Project_Name of Projects loop
         declare
            Name : constant String := To_String (Project_Name);
         begin
            if Project_Name /= "" then
               Put_Line
                 ("Focusing on the " & Name & " sub-project");
               Project := Tree.Project_From_Name (Name);
               pragma Assert (Project /= No_Project);
               New_Line;
            end if;
         end;

         begin
            declare
               Data : constant Preprocessor_Data :=
                 Extract_Preprocessor_Data_From_Project (Tree, Project);
            begin
               Dump (Data);
            end;
         exception
            when Exc : File_Read_Error | Syntax_Error =>
               Put_Line
                 (Exception_Name (Exc) & ": " & Exception_Message (Exc));
               New_Line;
         enD;
      end loop;

      Tree.Unload;
      Free (Env);
   end Process;

begin
   Process ("simple/p.gpr");
   Process ("with/q.gpr");
   Process ("extends/q.gpr");
   Process
     ("aggr/aggr.gpr",
      (Null_Unbounded_String,
       To_Unbounded_String ("p"),
       To_Unbounded_String ("q")));
   Process ("file_read_error/p.gpr");
   Process ("syntax_error/p.gpr");
   Put_Line ("Done.");
end Main;
