with Ada.Directories;       use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is
   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);
   function Visit (Node : Ada_Node'Class) return Visit_Status;
   function Identify (CU : Compilation_Unit) return String;

   package App is new Libadalang.Helpers.App
     (Name         => "example",
      Description  => "Example app",
      Process_Unit => Process_Unit);

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit) is
      pragma Unreferenced (Context);
   begin
      Unit.Root.Traverse (Visit'Access);
      New_Line;
   end Process_Unit;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Ada_Node'Class) return Visit_Status is
      CU : Compilation_Unit;
   begin
      if Node.Kind /= Ada_Compilation_Unit then
         return Into;
      end if;

      CU := Node.As_Compilation_Unit;
      Put_Line ("Unit dependencies of " & Identify (CU) & ":");
      for Dep of CU.P_Unit_Dependencies loop
         Put_Line ("  - " & Identify (Dep));
      end loop;
      return Over;
   end Visit;

   --------------
   -- Identify --
   --------------

   function Identify (CU : Compilation_Unit) return String is
      Result : Unbounded_String;
   begin
      for N of CU.P_Syntactic_Fully_Qualified_Name loop
         if Length (Result) > 0 then
            Append (Result, '.');
         end if;
         Append (Result, Image (To_Text (N)));
      end loop;
      Append (Result, " (from " & Simple_Name (CU.Unit.Get_Filename) & ")");
      return To_String (Result);
   end Identify;

begin
   App.Run;
   Put_Line ("Done");
end Main;
