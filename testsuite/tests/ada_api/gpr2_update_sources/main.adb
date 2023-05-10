--  Check that creating a unit provider based on a GPR2 project automatically
--  calls GPR2.Project.Tree.Update_Sources. Libadalang used not to do it, which
--  prevented GPR2 from correctly resolving units.

with Ada.Directories; use Ada.Directories;
with Ada.Text_IO;     use Ada.Text_IO;

with GPR2.Context;
with GPR2.Path_Name;
with GPR2.Project.Tree;

with Libadalang.Analysis;         use Libadalang.Analysis;
with Libadalang.Common;           use Libadalang.Common;
with Libadalang.Project_Provider; use Libadalang.Project_Provider;

procedure Main is

   UFP : Unit_Provider_Reference;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project (Tree : in out GPR2.Project.Tree.Object) is
   begin
      Tree.Load_Autoconf
        (Filename => GPR2.Path_Name.Create_File ("p.gpr"),
         Context  => GPR2.Context.Empty);
   end Load_Project;

   -----------
   -- Check --
   -----------

   procedure Check is
      Ctx : constant Analysis_Context := Create_Context (Unit_Provider => UFP);
      U   : constant Analysis_Unit :=
        Ctx.Get_From_Provider ("ada.text_io", Unit_Specification);
   begin
      for D of U.Diagnostics loop
         Put_Line (U.Format_GNU_Diagnostic (D));
      end loop;
      Put_Line ("Got " & Simple_Name (U.Get_Filename));
   end Check;

begin

   Put_Line ("== Simple project provider ==");
   New_Line;
   declare
      Tree : GPR2.Project.Tree.Object;
   begin
      Load_Project (Tree);
      UFP := Create_Project_Unit_Provider (Tree);
      Check;
   end;
   New_Line;

   Put_Line ("== Partition project provider ==");
   New_Line;
   declare
      Tree      : GPR2.Project.Tree.Object;
      Partition : GPR2_Provider_And_Projects_Array_Access;
   begin
      Load_Project (Tree);
      Partition := Create_Project_Unit_Providers (Tree);
      if Partition'Length /= 1 then
         raise Program_Error;
      end if;
      UFP := Partition.all (1).Provider;
      Free (Partition);
      Check;
   end;
   New_Line;

   Put_Line ("Done.");
end Main;
