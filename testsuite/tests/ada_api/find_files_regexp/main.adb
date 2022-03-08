--  Check that ``Libadalang.Auto_Provider.Find_Files_Regexp`` works as expected

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regexp; use GNAT.Regexp;

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Auto_Provider; use Libadalang.Auto_Provider;

procedure Main is
   R     : constant Regexp := Compile ("m*.adb", Glob => True);
   Files : File_Array_Access := Find_Files_Regexp (R, (1 => Create (+".")));
begin
   for F of Files.all loop
      Put_Line (+F.Base_Name);
   end loop;
   Unchecked_Free (Files);
end Main;
