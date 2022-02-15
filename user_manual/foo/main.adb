with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Common;        use Libadalang.Common;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

procedure Main is

   function Process (Node : Ada_Node'Class) return Visit_Status;

   -------------
   -- Process --
   -------------

   function Process (Node : Ada_Node'Class) return Visit_Status is
   begin
      if Node.Text = "Name" then
         Put_Line (Node.Image
                   & " resolves to "
                   & Node.As_Identifier.P_Referenced_Decl.Image);
      end if;
      return Into;
   end Process;

   Ctx : constant Analysis_Context := Create_Context
     (File_Reader => Create_Preprocessor_From_File ("prep-data.txt"));
   U   : constant Analysis_Unit := Ctx.Get_From_File ("print_os.adb");
begin
   U.Root.Traverse (Process'Access);
end Main;
