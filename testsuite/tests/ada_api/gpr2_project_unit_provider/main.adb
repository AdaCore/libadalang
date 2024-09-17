--  Test that Libadalang's project unit provider behaves as expected. First
--  check that unsupported projects are properly rejected, then load a
--  supported one an check that name resolution properly uses it.

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;

with GPR2.Options;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
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
      Options : GPR2.Options.Object;
      View    : GPR2.Project.View.Object;
   begin
      Put_Line ("Loading " & File & "...");
      if Project'Length > 0 then
         Put_Line ("   Targetting subproject " & Project);
      end if;
      Options.Add_Switch (GPR2.Options.P, File);
      if not Tree.Load
        (Options,
         With_Runtime         => True,
         Artifacts_Info_Level => GPR2.Sources_Units)
      then
         raise Program_Error;
      end if;
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

   procedure Resolve
     (Project   : String;
      Unit_Name : Text_Type;
      Kind      : Analysis_Unit_Kind := Unit_Specification)
   is
      function Process (N : Ada_Node'Class) return Visit_Status;
      --  If ``N`` has a reference whose resolution must be tested, do it

      -------------
      -- Process --
      -------------

      function Process (N : Ada_Node'Class) return Visit_Status is
         Ref, Decl : Ada_Node;
      begin
         Ref := N.As_Ada_Node;
         case N.Kind is
            when Ada_Object_Decl =>
               Ref :=
                 N
                 .As_Object_Decl
                 .F_Type_Expr
                 .As_Ada_Node;
               Decl :=
                 Ref
                 .As_Subtype_Indication
                 .P_Designated_Type_Decl_From (N.As_Ada_Node)
                 .As_Ada_Node;

            when Ada_Subp_Body_Stub =>
               Decl := Ref.As_Subp_Body_Stub.P_Next_Part_For_Decl.As_Ada_Node;

            when others =>
               Ref := No_Ada_Node;
         end case;

         if not Ref.Is_Null then
            Put_Line (Ref.Image & " resolves to:");
            Put_Line ("  " & Decl.Image);
         end if;

         return Into;
      end Process;
   begin
      Put_Line ("== Resolutions in " & Project & " ==");
      New_Line;
      declare
         Ctx  : constant Analysis_Context :=
            Create_Context (Unit_Provider => Load_Project (Project));
         Unit : constant Analysis_Unit :=
            Get_From_Provider (Ctx, Unit_Name, Kind);
      begin
         Unit.Root.Traverse (Process'Access);
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
   Resolve ("p.gpr", "p3", Unit_Body);
   Resolve ("multi_unit_files_1.gpr", "objects");
   Resolve ("multi_unit_files_2.gpr", "objects");
   Resolve ("extending.gpr", "ext");

   Put_Line ("Done.");
end Main;
