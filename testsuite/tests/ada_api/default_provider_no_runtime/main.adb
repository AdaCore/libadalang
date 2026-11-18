--  Check that the default provider works without a native Ada toolchain

with Ada.Environment_Variables;
with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is
   Ctx  : Analysis_Context;
   Unit : Analysis_Unit;
   T    : Type_Expr;
begin
   --  Make the native toolchain unavailable so that LibGPR2 cannot find the
   --  native Ada toolchain.

   Ada.Environment_Variables.Set ("PATH", "");

   --  Create a context with the default provider, then make sure this provider
   --  1) resolves user units as expected 2) but cannot find runtime units.

   Ctx := Create_Context
            (Event_Handler =>
               Command_Line_Event_Handler
                 (Keep_Going_On_Missing_Dependency => True));
   Unit := Ctx.Get_From_Provider ("foo", Unit_Specification);
   Put_Line ("Unit root: " & Unit.Root.Image);

   T := Unit.Root.Lookup ((4, 6)).As_Object_Decl.F_Type_Expr;
   Put_Line (T.Image & " resolves to: " & T.P_Designated_Type_Decl.Image);

   Put_Line ("Done.");
end Main;
