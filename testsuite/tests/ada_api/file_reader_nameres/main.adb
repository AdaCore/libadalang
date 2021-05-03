with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

with Support; use Support;

procedure Main is

   procedure Process (Filename : String);
   --  Parse the Filename source file and display its diagnostics. If there are
   --  none, resolve all identifiers in it.

   function Visit (Node : Ada_Node'Class) return Visit_Status;
   --  If Node is an identifier, try to print its referenced decl. Return Into
   --  in any case.

   Ctx  : constant Analysis_Context :=
     Create_Context (File_Reader => Create_My_FR);

   -------------
   -- Process --
   -------------

   procedure Process (Filename : String) is
      Unit : constant Analysis_Unit := Get_From_File (Ctx, Filename);
   begin
      Put_Line ("== " & Filename & " ==");
      New_Line;
      if Unit.Has_Diagnostics then
         for D of Unit.Diagnostics loop
            Put_Line (Unit.Format_GNU_Diagnostic (D));
         end loop;
         New_Line;
         return;
      end if;

      Unit.Root.Traverse (Visit'Access);
      New_Line;
   end Process;

   -----------
   -- Visit --
   -----------

   function Visit (Node : Ada_Node'Class) return Visit_Status is
   begin
      if Node.Kind = Ada_Identifier then
         Put ("Resolving " & Node.Image & ": ");
         declare
            Decl : Basic_Decl;
         begin
            Decl := Node.As_Identifier.P_Referenced_Decl;
            Put_Line (Decl.Image);
         exception
            when Property_Error =>
               Put_Line ("Property_Error");
         end;
      end if;
      return Into;
   end Visit;

begin
   Process ("test.adb");
   Process ("foo.adb");
end Main;
