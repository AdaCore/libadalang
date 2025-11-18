--  Check that ``Libadalang.Config_Pragmas.Set_Mapping`` rejects invalid inputs
--  as expected.

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Config_Pragmas; use Libadalang.Config_Pragmas;

procedure Invalid is

   Ctx : constant Analysis_Context := Create_Context;

   procedure Check_Error
     (Label   : String;
      Context : Analysis_Context;
      Mapping : Config_Pragmas_Mapping);
   --  Run ``Set_Mapping (Context, Mapping)`` and print if it raises an
   --  exception.

   -----------------
   -- Check_Error --
   -----------------

   procedure Check_Error
     (Label   : String;
      Context : Analysis_Context;
      Mapping : Config_Pragmas_Mapping) is
   begin
      Put_Line ("== " & Label & "==");
      New_Line;
      begin
         Set_Mapping (Context, Mapping);
         Put_Line ("No exception...");
      exception
         when Exc : Precondition_Failure =>
            Put_Line (Exception_Name (Exc) & ": " & Exception_Message (Exc));
      end;
      New_Line;
   end Check_Error;

   Mapping : Config_Pragmas_Mapping;
   Key     : constant Unbounded_String := To_Unbounded_String ("key");
   Value   : constant Unbounded_String := To_Unbounded_String ("value");
begin
   Mapping.Local_Pragmas.Include (Key, Value);

   Check_Error ("no context", No_Analysis_Context, Mapping);

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (Null_Unbounded_String, Value);
      Check_Error ("null key unit", Ctx, M);
   end;

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (Key, Null_Unbounded_String);
      Check_Error ("null value unit", Ctx, M);
   end;

   Put_Line ("Done.");
end Invalid;
