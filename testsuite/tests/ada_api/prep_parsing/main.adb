with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is
   Ctx : constant Analysis_Context := Create_Context;
   U   : constant Analysis_Unit := Ctx.Get_From_File ("foo.ads");

   procedure Check (Label : String);

   -----------
   -- Check --
   -----------

   procedure Check (Label : String) is
   begin
      Put_Line ("== " & Label & " ==");
      New_Line;
      U.Reparse;
      if U.Has_Diagnostics then
         Put_Line ("Diagnostics:");
         for D of U.Diagnostics loop
            Put_Line ("  " & U.Format_GNU_Diagnostic (D));
         end loop;
      else
         Put_Line ("No diagnostic");
      end if;
      New_Line;
   end Check;
begin
   Check ("Default");
   Ctx.Disable_Preprocessor_Directives_Errors;
   Check ("Errors disabled");
   Put_Line ("Done");
end Main;
