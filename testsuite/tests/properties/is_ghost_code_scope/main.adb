with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Traces;

with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is
   U : constant Analysis_Unit := Create_Context.Get_From_File ("test.adb");
   N : constant Ada_Node := U.Root.Lookup ((6, 4)).Parent.Parent.Parent;
begin
   --  Enabling the trace will let us check whether PLE is ever triggered on
   --  `pkg.adb`. We expect it not to be, because it's not necessary to
   --  determine whether procedure `Foo` is ghost or not (since the
   --  aspect must be set on the spec).
   GNATCOLL.Traces.Parse_Config ("LIBADALANG.MAIN_TRACE=yes" & ASCII.LF);
   Put_Line (N.Image & " is ghost? " & N.As_Call_Stmt.P_Is_Ghost_Code'Image);
end Main;

