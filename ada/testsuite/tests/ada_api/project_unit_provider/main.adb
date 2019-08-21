--  Test that Libadalang's project unit provider behaves as expected. First
--  check that unsupported projects are properly rejected, then load a
--  supported one an check that name resolution properly uses it.

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects; use GNATCOLL.Projects;
with GNATCOLL.VFS;      use GNATCOLL.VFS;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   function Load_Project
     (File : String; Project : String := "") return Unit_Provider_Reference;
   procedure Try_Loading_Project (File : String; Project : String := "");

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (File : String; Project : String := "") return Unit_Provider_Reference
   is
      Env  : Project_Environment_Access;
      Tree : Project_Tree_Access := new Project_Tree;
      Prj  : Project_Type := No_Project;
   begin
      Put_Line ("Loading " & File & "...");
      if Project'Length > 0 then
         Put_Line ("   Targetting subproject " & Project);
      end if;
      Initialize (Env);
      Load (Tree.all, Create (+File), Env);
      if Project'Length > 0 then
         Prj := Tree.Project_From_Name (Project);
         pragma Assert (Prj /= No_Project);
      end if;
      return Create_Project_Unit_Provider_Reference (Tree, Prj, Env, True);
   exception
      when others =>
         Prj := No_Project;
         Tree.Unload;
         Free (Tree);
         Free (Env);
         raise;
   end Load_Project;

   -------------------------
   -- Try_Loading_Project --
   -------------------------

   procedure Try_Loading_Project (File : String; Project : String := "") is
      Dummy : Unit_Provider_Reference;
   begin
      Dummy := Load_Project (File, Project);
      Put_Line ("   Success");
   exception
      when Exc : Invalid_Project =>
         Put_Line ("   Invalid_Project exception: "
                   & Ada.Exceptions.Exception_Message (Exc));
   end Try_Loading_Project;

begin
   Try_Loading_Project ("unsupported_aggr.gpr");
   Try_Loading_Project ("unsupported_aggr.gpr", "p");
   Try_Loading_Project ("supported_simple_aggr.gpr");
   Try_Loading_Project ("supported_chained_aggr.gpr");

   declare
      Ctx  : constant Analysis_Context :=
         Create_Context (Unit_Provider => Load_Project ("p.gpr"));
      Unit : constant Analysis_Unit :=
         Get_From_Provider (Ctx, "p2", Unit_Specification);
      Root : constant Ada_Node := Unit.Root;

      Subtype_Ind : constant Subtype_Indication := Find_First
        (Root, Kind_Is (Ada_Subtype_Indication)).As_Subtype_Indication;
      Res_Type    : constant Ada_Node_Array :=
         Subtype_Ind.F_Name.P_Matching_Nodes;
   begin
      Put_Line (Subtype_Ind.Short_Image & " resolves to:");
      for E of Res_Type loop
         Put_Line ("  " & E.Short_Image);
      end loop;
   end;

   Put_Line ("Done.");
end Main;
