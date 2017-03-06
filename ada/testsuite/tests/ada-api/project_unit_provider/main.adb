with Ada.Text_IO;                use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;            use Libadalang.Analysis;
with Libadalang.Unit_Files;          use Libadalang.Unit_Files;
with Libadalang.Unit_Files.Projects; use Libadalang.Unit_Files.Projects;

procedure Main is

   function Load_Project (File : String) return Unit_Provider_Access;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project (File : String) return Unit_Provider_Access is
      Env     : Project_Environment_Access;
      Project : constant Project_Tree_Access := new Project_Tree;
   begin
      Initialize (Env);
      Load (Project.all, Create (+File), Env);
      return new Project_Unit_Provider_Type'(Create (Project, Env, True));
   end Load_Project;

   UFP : Unit_Provider_Access := Load_Project ("p.gpr");
   Ctx : Analysis_Context :=
      Create (Unit_Provider => Unit_Provider_Access_Cst (UFP));

   Unit : constant Analysis_Unit :=
      Get_From_Provider (Ctx, "p2", Unit_Specification);
   Root : constant Ada_Node := Libadalang.Analysis.Root (Unit);

begin
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
   Destroy (UFP);
   Put_Line ("Done.");
end Main;
