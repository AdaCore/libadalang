--  Calling "P_Has_Restriction" on the compilation unit for a subprogram body
--  used to emit a "missing file is error" event for the subprogram spec,
--  whereas a subprogram spec is not required in that case.
--
--  This test check that we do not complain about the missing "foo.ads" source
--  file.

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

with Support; use Support;

procedure Main is
   EH    : constant Event_Handler_Reference :=
     Create_Event_Handler_Reference (My_EH'(null record));
   Ctx   : constant Analysis_Context := Create_Context (Event_Handler => EH);
   U     : constant Analysis_Unit := Ctx.Get_From_File ("foo.adb");
   CU    : constant Compilation_Unit := U.Root.As_Compilation_Unit;
   Dummy : constant Boolean :=
     CU.P_Has_Restriction (To_Unbounded_Text ("No_Elaboration_Code"));
begin
   null;
end Main;
