--  Check that ``Libadalang.Config_Pragmas.Set_Mapping`` rejects invalid inputs
--  as expected.

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Config_Pragmas; use Libadalang.Config_Pragmas;

procedure Invalid is

   Ctx   : constant Analysis_Context := Create_Context;
   Ctx_2 : constant Analysis_Context := Create_Context;

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
   Key     : constant Analysis_Unit := Ctx.Get_From_File ("key");
   Value   : constant Analysis_Unit := Ctx.Get_From_File ("value");
begin
   Mapping.Local_Pragmas.Include (Key, Value);

   Check_Error ("no context", No_Analysis_Context, Mapping);

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (Ctx_2.Get_From_File ("key2"), Value);
      Check_Error ("foreign key unit", Ctx, M);
   end;

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (Key, Ctx_2.Get_From_File ("value2"));
      Check_Error ("foreign value unit", Ctx, M);
   end;

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Global_Pragmas := Ctx_2.Get_From_File ("glob");
      Check_Error ("foreign global", Ctx, M);
   end;

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (No_Analysis_Unit, Value);
      Check_Error ("null key unit", Ctx, M);
   end;

   declare
      M : Config_Pragmas_Mapping := Mapping;
   begin
      M.Local_Pragmas.Include (Key, No_Analysis_Unit);
      Check_Error ("null value unit", Ctx, M);
   end;

   Put_Line ("Done.");
end Invalid;
