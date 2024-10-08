with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Errors; use Langkit_Support.Errors;
with Libadalang.Target_Info; use Libadalang.Target_Info;

procedure Main is

   procedure Check_Loading (Config : String);
   --  Try to load a target information file. On error, print the exception
   --  message. On success, dump the target information.

   -------------------
   -- Check_Loading --
   -------------------

   procedure Check_Loading (Config : String) is
      TI : Target_Information;
   begin
      Put_Line ("== " & Config & " ==");
      New_Line;
      begin
         TI := Load (Config & ".txt");
      exception
         when Use_Error | Name_Error =>
            Put_Line ("IO error");
            New_Line;
            return;

         when Exc : Invalid_Input =>
            Put_Line ("Invalid_Input: " & Exception_Message (Exc));
            New_Line;
            return;
      end;
      Dump (TI);
      New_Line;
   end Check_Loading;

begin
   Check_Loading ("invalid no_such_file");
   Check_Loading ("invalid empty");
   Check_Loading ("invalid_var_syntax");
   Check_Loading ("invalid_var_name");
   Check_Loading ("invalid_line_too_long");
   Check_Loading ("invalid_boolean");
   Check_Loading ("invalid_positive");
   Check_Loading ("invalid_too_large");
   Check_Loading ("invalid_float_syntax");
   Check_Loading ("invalid_float_digits");
   Check_Loading ("invalid_float_repr");
   Check_Loading ("invalid_float_size");
   Check_Loading ("invalid_float_alignment");
   Check_Loading ("invalid_missing_double");
   Check_Loading ("linux_64");

   Put_Line ("Done.");
end Main;
