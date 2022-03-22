--  Calling "P_Next_Part_On_Decl" on a generic package instantation should not
--  emit a "missing file is error" event, as instantiations do not have bodies.

with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

with Support; use Support;

procedure Main is
   EH    : constant Event_Handler_Reference :=
     Create_Event_Handler_Reference (My_EH'(null record));
   Ctx   : constant Analysis_Context := Create_Context (Event_Handler => EH);
   U     : constant Analysis_Unit := Ctx.Get_From_File ("pkg.ads");
   Pkg   : constant Basic_Decl := U.Root.As_Compilation_Unit.P_Decl;
   NP    : Basic_Decl;
begin
   Put_Line ("Trying to get the next part for " & Pkg.Image);
   NP := Pkg.P_Next_Part_For_Decl;
   Put_Line ("  -> " & NP.Image);
   Put_Line ("Done.");
end Main;
