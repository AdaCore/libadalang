with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

package Support is

   function Create_My_FR return File_Reader_Reference;
   --  Return a dummy file reader that pretends there is only one possible
   --  source file: "foo.adb".

end Support;
