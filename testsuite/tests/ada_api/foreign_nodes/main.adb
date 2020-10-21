--  This testcase tries to make sure that foreign nodes/exiled entries in
--  analysis units are properly updated after various analysis units reloads.

with Ada.Text_IO; use Ada.Text_IO;

with Langkit_Support.Diagnostics;
with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is

   Ctx   : constant Analysis_Context := Create_Context;
   function Get (Filename : String) return Analysis_Unit;
   procedure Dump;

   Pkg   : Analysis_Unit;
   Child : Analysis_Unit;

   ---------
   -- Get --
   ---------

   function Get (Filename : String) return Analysis_Unit is
      Unit : constant Analysis_Unit :=
         Get_From_File (Ctx, Filename, Reparse => True);
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
      Populate_Lexical_Env (Unit);
      return Unit;
   end Get;

   ----------
   -- Dump --
   ----------

   procedure Dump is
   begin
      Put_Line ("## Pkg ##");
      Dump_Lexical_Env (Pkg);
      Put_Line ("## Child ##");
      Dump_Lexical_Env (Child);
   end Dump;

begin
   --  Load both packages a first time
   Pkg   := Get ("pkg.ads");
   Child := Get ("pkg-child.ads");

   --  Reload Child, so that the foreign node (Pkg.Child) in Pkg becomes a
   --  dangling reference.
   Child := Get ("pkg-child.ads");

   --  Then reload Pkg. If foreign nodes handling is correct, this dangling
   --  reference should have been removed when reaching this statement.
   --  Otherwise, we will end up trying to access this node, and thus Valgrind
   --  will report an invalid read error (crash is not guaranteed in case of
   --  memory corruption...).
   Put_Line ("===================");
   Put_Line ("== Reloading Pkg ==");
   Put_Line ("===================");
   Pkg := Get ("pkg.ads");
   Dump;

   --  Then reload Pkg, so that this time we test exiled entries
   Put_Line ("=====================");
   Put_Line ("== Reloading Child ==");
   Put_Line ("=====================");
   Child := Get ("pkg-child.ads");
   Dump;

   Put_Line ("Done.");
end Main;
