with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   function Visit (Node : Ada_Node'Class) return Visit_Status;

   package App is new Libadalang.Helpers.App
     (Name         => "example",
      Description  => "Example app",
      Process_Unit => Process_Unit);

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
   begin
      Unit.Root.Traverse (Visit'Access);
      New_Line;
   end Process_Unit;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Ada_Node'Class) return Visit_Status is
   begin
      case Node.Kind is
         when Ada_Subp_Body | Ada_Subp_Body_Stub =>
            declare
               N : constant Body_Node := Node.As_Body_Node;
            begin
               Put_Line ("== " & N.Image & " ==");
               New_Line;
               Put_Line ("Previous part: " & N.P_Previous_Part.Image);
               Put_Line ("Next part: " & N.P_Next_Part_For_Decl.Image);
               Put_Line ("Decl part: " & N.P_Decl_Part.Image);
               New_Line;
            end;

         when Ada_Subp_Decl =>
            declare
               N : constant Subp_Decl := Node.As_Subp_Decl;
            begin
               Put_Line ("== " & N.Image & " ==");
               New_Line;
               Put_Line ("Body part: " & N.P_Body_Part.Image);
               New_Line;
            end;

         when others =>
            null;
      end case;
      return Into;
   end Visit;

begin
   App.Run;
   Put_Line ("Done");
end Main;
