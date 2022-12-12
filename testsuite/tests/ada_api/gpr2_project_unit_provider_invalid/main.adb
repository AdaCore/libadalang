with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   function Load_Project (File : String) return Unit_Provider_Reference;

   function "+" (S : Wide_Wide_String) return Unbounded_Wide_Wide_String
     renames To_Unbounded_Wide_Wide_String;
   function "+" (S : Unbounded_Wide_Wide_String) return Wide_Wide_String
     renames To_Wide_Wide_String;

   Tree : GPR2.Project.Tree.Object;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (File : String) return Unit_Provider_Reference is
   begin
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File (GPR2.Filename_Type (File)),
         Context  => GPR2.Context.Empty);
      return Create_Project_Unit_Provider (Tree, Tree.Root_Project);
   end Load_Project;

   Ctx  : constant Analysis_Context :=
     Create_Context (Unit_Provider => Load_Project ("p.gpr"));
   Unit : Analysis_Unit;

   Filenames : constant array (Positive range <>)
                        of Unbounded_Wide_Wide_String :=
     (+(1 => Wide_Wide_Character'Val (233)),
      +"unknown_unit");

begin
   for File of Filenames loop
      Put_Line ("Trying to get unit: " & Image (+File, With_Quotes => True));
      begin
         Unit := Get_From_Provider (Ctx, +File, Unit_Specification);

         Put_Line ("   ...   got no exception.");
         if Has_Diagnostics (Unit) then
            Put_Line ("   ...   but we got diagnostics:");
            for D of Diagnostics (Unit) loop
               Put_Line ("   " & To_Pretty_String (D));
            end loop;
         end if;
      exception
         when Exc : Invalid_Unit_Name_Error =>
            Put_Line ("   ... got an exception:");
            Put_Line ("   " & Exception_Name (Exc));
            Put_Line ("   " & Exception_Message (Exc));
      end;
   end loop;

   Put_Line ("Done.");
end Main;
