with Ada.Text_IO;                use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;            use Libadalang.Analysis;
with Libadalang.AST;                 use Libadalang.AST;
with Libadalang.AST.Types;           use Libadalang.AST.Types;
with Libadalang.Unit_Files.Projects; use Libadalang.Unit_Files.Projects;

procedure Main is

   function Load_Project (File : String) return Project_Tree_Access;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (File : String) return Project_Tree_Access is
      Result : constant Project_Tree_Access := new Project_Tree;
   begin
      Load (Result.all, Create (+File));
      return Result;
   end Load_Project;

   Prj    : Project_Tree_Access := Load_Project ("p.gpr");
   UFP    : Project_Unit_File_Provider_Type := Create (Prj);
   Ctx    : Analysis_Context :=
      Create (Unit_File_Provider => UFP'Unrestricted_Access);

   File   : constant String := "src2/p2.ada_specification";
   Unit   : Analysis_Unit := Get_From_File (Ctx, File);
   Root   : constant Ada_Node := Libadalang.Analysis.Root (Unit);

begin
   if Root = null then
      Put_Line ("Could not parse " & File);
      return;
   end if;

   Populate_Lexical_Env (Unit);

   declare
      Subtype_Ind : constant Subtype_Indication := Subtype_Indication
        (Root.Find_First
           (new Ada_Node_Kind_Filter'(Kind => Ada_Subtype_Indication)));
      Res_Type    : Ada_Node_Array_Access := Subtype_Ind.F_Name.P_Entities;
   begin
      Put_Line (Image (Subtype_Ind.Short_Image) & " resolves to:");
      for E of Res_Type.Items loop
         Put_Line ("  " & Image (E.Short_Image));
      end loop;
      Dec_Ref (Res_Type);
   end;

   Destroy (Ctx);
   Free (Prj);
   Put_Line ("Done.");
end Main;
