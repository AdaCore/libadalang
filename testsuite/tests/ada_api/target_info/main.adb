with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Langkit_Support.Text;   use Langkit_Support.Text;
with Libadalang.Analysis;    use Libadalang.Analysis;
with Libadalang.Common;      use Libadalang.Common;
with Libadalang.Expr_Eval;   use Libadalang.Expr_Eval;
with Libadalang.Iterators;   use Libadalang.Iterators;
with Libadalang.Target_Info; use Libadalang.Target_Info;

procedure Main is

   procedure Check_Loading (Config : String);
   --  Try to load a target information file. On error, print the exception
   --  message. On success, dump the target information.

   procedure Check_Standard (Config : String);
   --  Load a target information file, then check on the related Ada source
   --  code that all conditions in Compile_Time_Error evaluate to False.

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

   --------------------
   -- Check_Standard --
   --------------------

   procedure Check_Standard (Config : String) is
      Ctx : Analysis_Context;
      U   : Analysis_Unit;
      TI  : Target_Information;
   begin
      Put_Line ("== " & Config & " ==");
      New_Line;

      --  Parse the Ada source code and check the absence of parsing errors

      Ctx := Create_Context;
      U := Ctx.Get_From_File (Config & ".ads");
      if U.Has_Diagnostics then
         for D of U.Diagnostics loop
            Put_Line (U.Format_GNU_Diagnostic (D));
         end loop;
         raise Program_Error;
      end if;

      --  Load the target information file and assign the target information to
      --  the analysis context.

      TI := Load (Config & ".txt");
      Ctx.Set_Target_Information (TI);

      --  Go through all Compile_Time_Error pragmas and check they do not
      --  trigger.

      for N of Find (U.Root, Kind_Is (Ada_Pragma_Node)).Consume loop
         declare
            P : constant Pragma_Node := N.As_Pragma_Node;
         begin
            if P.F_Id.Text /= "Compile_Time_Error" then
               raise Program_Error;
            elsif P.F_Args.Children_Count /= 2 then
               raise Program_Error;
            end if;

            declare
               use type Libadalang.Expr_Eval.Big_Integer;

               Label  : constant String := Image (P.F_Args.Child (2).Text);
               Cond   : constant Eval_Result :=
                 Expr_Eval
                   (P.F_Args.Child (1).As_Pragma_Argument_Assoc.F_Expr);

               --  Compiler_Time_Error triggers when its condition is True
               --  (1, FAIL), so we expect False (0, OK).

               Result : constant String :=
                 (if As_Int (Cond) = 0
                  then "OK  "
                  elsif As_Int (Cond) = 1
                  then "FAIL"
                  else raise Program_Error with
                         Image (Cond) & ".As_Int = " & As_Int (Cond).Image);
            begin
               Put_Line (Result & " " & Label);
            end;
         end;
      end loop;
      New_Line;
   end Check_Standard;

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

   Check_Standard ("linux_64");
   Check_Standard ("linux_32");
   Check_Standard ("arm_stm32");
   Check_Standard ("small_ints");

   Put_Line ("Done.");
end Main;
