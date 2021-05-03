with Ada.Directories;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

package body Support is

   type My_File_Reader is new File_Reader_Interface with null record;

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector);

   overriding procedure Release (Self : in out My_File_Reader) is null;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Self        : My_File_Reader;
      Filename    : String;
      Charset     : String;
      Read_BOM    : Boolean;
      Contents    : out Decoded_File_Contents;
      Diagnostics : in out Diagnostics_Vectors.Vector)
   is
   begin
      if Ada.Directories.Simple_Name (Filename) /= "foo.adb" then
         Append (Diagnostics, Message => "no such source file");
         return;
      end if;

      Direct_Read (Filename, Charset, Read_BOM, Contents, Diagnostics);
   end Read;

   ------------------
   -- Create_My_FR --
   ------------------

   function Create_My_FR return File_Reader_Reference is
      FR : constant My_File_Reader := (null record);
   begin
      return Create_File_Reader_Reference (FR);
   end Create_My_FR;

end Support;
