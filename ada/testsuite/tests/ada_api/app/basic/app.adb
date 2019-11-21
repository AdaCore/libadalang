with Ada.Text_IO; use Ada.Text_IO;
with Libadalang.Common; use Libadalang.Common;

package body App is

   procedure Process_Unit (Unit : Analysis_Unit) is
      function Visit (Node : Ada_Node'Class) return Visit_Status;

      function Visit (Node : Ada_Node'Class) return Visit_Status is
      begin
         case Node.Kind is
         when Ada_Goto_Stmt =>
            Put_Line ("Found goto stmt: " & Node.Short_Image);
            return Over;
         when others =>
            return Into;
         end case;
      end Visit;
   begin
      Unit.Root.Traverse (Visit'Access);
   end Process_Unit;
end App;
