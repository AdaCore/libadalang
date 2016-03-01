with Ada.Command_Line;                use Ada.Command_Line;
with Ada.Exceptions;                  use Ada.Exceptions;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Text_IO;                     use Ada.Text_IO;

with Interfaces;
use type Interfaces.Unsigned_16;

with Langkit_Support.Diagnostics;       use Langkit_Support.Diagnostics;
with Langkit_Support.Text;              use Langkit_Support.Text;
with Langkit_Support.Tokens;            use Langkit_Support.Tokens;
with Libadalang.Analysis;               use Libadalang.Analysis;
with Libadalang.AST;                    use Libadalang.AST;
with Libadalang.AST.Types.Parsers.Dump; use Libadalang.AST.Types.Parsers.Dump;
with Libadalang.AST.Types.Parsers.Test; use Libadalang.AST.Types.Parsers.Test;

procedure ASTEval is

   procedure Print_Usage;
   --  Display on standard output how this program should be used

   procedure Put_Quoted_Code
     (Code       : String;
      Sloc_Range : Source_Location_Range);
   --  Display on standard output the input code (assumed to be a single-line)
   --  and some ASCII-art to highlight the area that Sloc_Range covers.

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage is
   begin
      Put_Line ("Usage: " & Command_Name & " source-file [query [...]]");
      Put_Line ("Parse ""source-file"" and print the result of each query.");
      New_Line;
      Put_Line ("Example:");
      Put_Line
        ("  " & Command_Name & " foo.adb ""Root.Find (Identifier).P_Name""");
   end Print_Usage;

   ---------------------
   -- Put_Quoted_Code --
   ---------------------

   procedure Put_Quoted_Code
     (Code       : String;
      Sloc_Range : Source_Location_Range)
   is
      Padding : constant Natural := Natural (Sloc_Range.Start_Column - 1);
      Size    : constant Natural :=
         Natural (Sloc_Range.End_Column - Sloc_Range.Start_Column);
   begin
      Put ("  ");
      Put_Line (Code);
      Put ("  ");
      Put ((1 .. Padding => ' '));
      Put ('^');
      if Size > 1 then
         Put ((1 .. Size - 2 => '-'));
         Put ('^');
      end if;
      New_Line;
   end Put_Quoted_Code;

   Ctx  : Analysis_Context;
   Unit : Analysis_Unit;

begin
   if Argument_Count < 2 then
      Print_Usage;
      return;
   end if;

   --  Create analysis context and get an analysis unit for the input source
   --  file.

   Ctx := Create ("utf-8");
   Unit := Get_From_File (Ctx, Argument (1));
   if Has_Diagnostics (Unit) then

      --  If there are parsing errors, there's no need to go further

      for D of Diagnostics (Unit) loop
         Put_Line (To_Pretty_String (D));
      end loop;

   else
      --  If parsing went fine, prepare ourselves for semantic analysis and
      --  evaluate all input expressions.

      Populate_Lexical_Env (Unit);

      for I in 2 .. Argument_Count loop
         if I > 2 then
            New_Line;
         end if;
         Put_Line ("== " & Argument (I) & " ==");
         declare
            Expr : constant Expression := Parse_Expression (Argument (I));
         begin
            --  If Parse_Expression had a parsing error, diagnostics are
            --  already supposed to be output, so there's nothing left to do.

            if Expr /= No_Expression then
               declare
                  Value : constant Eval_Result :=
                     Eval (Expr, Libadalang.Analysis.Root (Unit));
               begin
                  if Value.Kind = Error_Value then
                     Put_Line ("error: " & To_String (Value.Message));
                     Put_Quoted_Code (Argument (I),
                                      Sloc_Range (Value.Sub_Expr));
                  else
                     Put_Line (Value);
                  end if;
               end;
            end if;
         end;
      end loop;
   end if;

   Destroy (Ctx);
end ASTEval;
