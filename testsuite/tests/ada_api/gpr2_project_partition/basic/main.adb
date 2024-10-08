--  Check how the project provider constructor builds partition of aggregated
--  projects.

with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;
with GPR2.Options;
with GPR2.Project.Tree;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Iterators;        use Libadalang.Iterators;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   package LAL renames Libadalang.Analysis;

   type Context_Array is array (Positive range <>) of Analysis_Context;

   Options : GPR2.Options.Object;
   Tree    : GPR2.Project.Tree.Object;
   PAPs    : GPR2_Provider_And_Projects_Array_Access;

begin
   GNATCOLL.Traces.Parse_Config ("LIBADALANG.PROJECT_PROVIDER.PARTITION=yes");
   Put_Line ("Loading the project:");
   Options.Add_Switch (GPR2.Options.P, "ap1.gpr");
   if not Tree.Load
     (Options,
      With_Runtime         => True,
      Artifacts_Info_Level => GPR2.Sources_Units)
   then
      raise Program_Error;
   end if;
   PAPs := Create_Project_Unit_Providers (Tree);

   declare
      Contexts : Context_Array (PAPs'Range);
   begin
      for I in PAPs'Range loop
         Contexts (I) := Create_Context (Unit_Provider => PAPs (I).Provider);
         Put ("  *");
         for View of PAPs (I).Projects loop
            Put (" ");
            Put (String (View.Name));
         end loop;
         New_Line;
      end loop;
      New_Line;
      Free (PAPs);

      declare
         Unit : constant Analysis_Unit :=
            Contexts (1).Get_From_Provider ("p2", Unit_Specification);
         Ref  : constant LAL.Name :=
            Find_First (Unit.Root, Kind_Is (Ada_Object_Decl))
            .As_Object_Decl.F_Renaming_Clause.F_Renamed_Object;
         Decl : constant Basic_Decl := Ref.P_Referenced_Decl;
      begin
         Put_Line (Ref.Image & " resolves to " & Decl.Image);
      end;
   end;

   Put_Line ("Done.");
end Main;
