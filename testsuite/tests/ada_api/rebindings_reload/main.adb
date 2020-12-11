--  This testcase tries to make sure that the lifecycle of env rebindings is
--  properly done when reparsing the various analysis units involved.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics;

with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.Common;    use Libadalang.Common;
with Libadalang.Iterators; use Libadalang.Iterators;

procedure Main is

   Ctx     : constant Analysis_Context := Create_Context;
   Foo     : constant Analysis_Unit := Get_From_File (Ctx, "foo.adb");
   Options : constant Analysis_Unit := Get_From_File (Ctx, "options.ads");
   Vectors : constant Analysis_Unit := Get_From_File (Ctx, "vectors.ads");

   ------------
   -- Reload --
   ------------

   procedure Reload (Unit : Analysis_Unit) is
   begin
      Reparse (Unit);
      if Has_Diagnostics (Unit) then
         for D of Diagnostics (Unit) loop
            Put_Line
              ("error " & Get_Filename (Unit) & ": "
               & Langkit_Support.Diagnostics.To_Pretty_String (D));
         end loop;
         raise Program_Error;
      end if;
   end Reload;

   -------------
   -- Process --
   -------------

   procedure Process (Title : String) is
      Call : constant Call_Expr := Find_First
        (Foo.Root, Kind_Is (Ada_Call_Expr)).As_Call_Expr;
   begin
      Put_Line ("== " & Title & " ==");

      declare
         Ref : constant Refd_Decl := Call.P_Failsafe_Referenced_Decl;
      begin
         if Kind (Ref) = Error then
            Put_Line ("Name resolution failed");
            New_Line;
            return;
         else
            Put_Line (Call.Image);
            Put_Line ("  resolves to: " & Decl (Ref).Image);
         end if;
      end;

      New_Line;
   end Process;

begin
   Reload (Foo);
   Reload (Options);
   Reload (Vectors);
   Process ("Initial");

   Reload (Foo);
   Process ("Reloading foo.adb");

   Reload (Options);
   Process ("Reloading options.ads");

   Reload (Vectors);
   Process ("Reloading vectors.ads");

   Put_Line ("Done.");
end Main;
