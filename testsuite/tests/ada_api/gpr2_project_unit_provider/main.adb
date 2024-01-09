--  Test that Libadalang's project unit provider behaves as expected. First
--  check that unsupported projects are properly rejected, then load a
--  supported one an check that name resolution properly uses it.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   function Load_Project
     (File : String; Project : String := "") return Unit_Provider_Reference;
   procedure Try_Loading_Project (File : String; Project : String := "");

   Tree : GPR2.Project.Tree.Object;

   ------------------
   -- Load_Project --
   ------------------

   function Load_Project
     (File : String; Project : String := "") return Unit_Provider_Reference
   is
      View : GPR2.Project.View.Object;
   begin
      Put_Line ("Loading " & File & "...");
      if Project'Length > 0 then
         Put_Line ("   Targetting subproject " & Project);
      end if;
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File (GPR2.Filename_Type (File)),
         Context  => GPR2.Context.Empty);
      Tree.Update_Sources (With_Runtime => True);
      if Project'Length > 0 then
         for V of Tree.Ordered_Views loop
            if To_Lower (String (V.Name)) = To_Lower (Project) then
               View := V;
               exit;
            end if;
         end loop;
         pragma Assert (View.Is_Defined);
      end if;
      return Create_Project_Unit_Provider (Tree, View);
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
      when Exc : GPR2.Project_Error =>
         Put_Line ("   GPR2.Project_Error exception: "
                   & Ada.Exceptions.Exception_Message (Exc));
      when Exc : Unsupported_View_Error =>
         Put_Line ("   Unsupported_View_Error exception: "
                   & Ada.Exceptions.Exception_Message (Exc));
   end Try_Loading_Project;

   -------------
   -- Resolve --
   -------------

   procedure Resolve (Project : String; Unit_Name : Text_Type) is
   begin
      Put_Line ("== Resolutions in " & Project & " ==");
      New_Line;
      declare
         Ctx  : constant Analysis_Context :=
            Create_Context (Unit_Provider => Load_Project (Project));
         Unit : constant Analysis_Unit :=
            Get_From_Provider (Ctx, Unit_Name, Unit_Specification);
         Root : constant Ada_Node := Unit.Root;

         Subtype_Ind : constant Subtype_Indication := Find_First
           (Root, Kind_Is (Ada_Subtype_Indication)).As_Subtype_Indication;
         Res_Type    : constant Ada_Node_Array :=
            Subtype_Ind.F_Name.P_Matching_Nodes;
      begin
         Put_Line (Subtype_Ind.Image & " resolves to:");
         for E of Res_Type loop
            Put_Line ("  " & E.Image);
         end loop;
      end;
      New_Line;
   end Resolve;

begin
   Try_Loading_Project ("unsupported_aggr.gpr");
   Try_Loading_Project ("unsupported_aggr.gpr", "unsupported_aggr");
   Try_Loading_Project ("unsupported_aggr.gpr", "p");
   Try_Loading_Project ("supported_no_conflict.gpr");
   Try_Loading_Project ("supported_simple_aggr.gpr");
   Try_Loading_Project ("supported_simple_aggr.gpr", "supported_simple_aggr");
   Try_Loading_Project ("supported_chained_aggr.gpr");
   Try_Loading_Project ("supported_chained_aggr.gpr",
                        "supported_chained_aggr");
   New_Line;

   Resolve ("p.gpr", "p2");
   Resolve ("multi_unit_files_1.gpr", "objects");
   Resolve ("multi_unit_files_2.gpr", "objects");
   Resolve ("extending.gpr", "ext");

   Put_Line ("Done.");
end Main;
