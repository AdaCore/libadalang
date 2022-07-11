with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Langkit_Support.Errors;   use Langkit_Support.Errors;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   procedure Process (Filename : String);

   -------------
   -- Process --
   -------------

   procedure Process (Filename : String) is
      Env  : Project_Environment_Access;
      Tree : Project_Tree;
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;

      Initialize (Env);
      Tree.Load (Create (+Filename));

      declare
         Data : constant Preprocessor_Data :=
           Extract_Preprocessor_Data_From_Project (Tree);
      begin
         Tree.Unload;
         Free (Env);

         Dump (Data);
      end;

   exception
      when Exc : File_Read_Error | Syntax_Error =>
         Tree.Unload;
         Free (Env);
         Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
         New_Line;
   end Process;

begin
   Process ("simple/p.gpr");
   Process ("with/q.gpr");
   Process ("extends/q.gpr");
   Process ("aggr/aggr.gpr");
   Process ("file_read_error/p.gpr");
   Process ("syntax_error/p.gpr");
   Put_Line ("Done.");
end Main;
