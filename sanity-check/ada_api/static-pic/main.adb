with Ada.Text_IO; use Ada.Text_IO;

with Libadalang.Analysis; use Libadalang.Analysis;

procedure Main is
   U : constant Analysis_Unit :=
     Create_Context.Get_From_Buffer
       (Filename => "foo.adb",
        Buffer   => "procedure Foo is begin null; end Foo;");
begin
   Put_Line (U.Root.Image);
end;
