--  Check that the ``P_All_Config_Pragmas`` property works as expected

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Langkit_Support.Text;   use Langkit_Support.Text;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Config_Pragmas; use Libadalang.Config_Pragmas;

procedure Sources is
   Ctx : constant Analysis_Context := Create_Context;

   procedure Check_Unit (Filename : String);
   --  Print the name of the given source file and the list of pragmas that
   --  apply to the corresponding unit.

   ----------------
   -- Check_Unit --
   ----------------

   procedure Check_Unit (Filename : String) is
      CU      : constant Compilation_Unit :=
        Ctx.Get_From_File (Filename).Root.As_Compilation_Unit;
      Pragmas : constant Pragma_Node_Array := CU.P_All_Config_Pragmas;
   begin
      Put (Filename & ":");
      for P of Pragmas loop
         Put (" " & Image (P.F_Id.Text));
      end loop;
      New_Line;
   end Check_Unit;
begin
   --  Check that calling these properties without setting configuration
   --  pragmas files mappings first creates an error.

   Put_Line
     ("Trying to get configuration pragmas without creating the mapping"
      & " first...");
   begin
      Check_Unit ("pkg.ads");
      raise Program_Error;
   exception
      when Exc : Property_Error =>
         Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
   end;
   New_Line;

   --  Now check that P_All_Config_Pragmas return the correct list of pragmas
   --  for each kind of unit.

   Set_Mapping
     (Ctx,
      (Local_Pragmas  => <>,
       Global_Pragmas => Ctx.Get_From_File ("global_foo.adc")));

   Check_Unit ("pkg.ads");
   Check_Unit ("pkg.adb");
   Check_Unit ("pkg-foo.adb");
   Check_Unit ("pkg-foo-bar.adb");
   New_Line;
   Put_Line ("Done.");
end Sources;
