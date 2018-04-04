--  This package provides the capability to automatically discover the layout
--  of source files in an Ada project, given a list of files, or a file name
--  pattern and a list of directories.
--
--  It is useful in order to easily run Libadalang on a complex project that
--  does not have its own GPR project file.

private with Ada.Containers.Hashed_Maps;
with Ada.Finalization;

with GNAT.Regpat;

with GNATCOLL.VFS;

private with Langkit_Support.Symbols;
private with Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Auto_Provider is

   Default_Source_Filename_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
      GNAT.Regpat.Compile (".*\.(ad.|a|spc|bdy)");

   function Find_Files
     (Name_Pattern : GNAT.Regpat.Pattern_Matcher :=
        Default_Source_Filename_Pattern;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of absolute file names for all regular files in the
   --  given Directories whose name match the given regular expression
   --  Name_Pattern. The result is dynamically allocated, so  the caller must
   --  free it when done with it.

   function Create_Auto_Provider
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset)
      return Unit_Provider_Interface'Class;
   --  Return a unit provider that knows which compilation units are to be
   --  found in which source files.
   --
   --  This knowledge is built trying to parse all given Input_Files as Ada
   --  source files and listing the compilation units found there. Files that
   --  cannot be parsed properly or redundant compilation units are discarded.
   --  Source files are decoded using the given Charset.
   --
   --  TODO??? Find a way to report discarded source files/compilation units.

   function Create_Auto_Provider
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset)
      return Unit_Provider_Access;
   --  Likewise, but return a heap-allocated value

private

   use Langkit_Support.Symbols;
   use Langkit_Support.Text;

   use GNATCOLL.VFS;

   package CU_To_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Virtual_File,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Auto_Unit_Provider_Type is limited new
      Ada.Finalization.Limited_Controlled
      and Libadalang.Analysis.Unit_Provider_Interface
   with record
      Keys    : Symbol_Table;
      Mapping : CU_To_File_Maps.Map;
   end record;

   type Auto_Unit_Provider_Access is access all Auto_Unit_Provider_Type;

   overriding procedure Initialize
     (Provider : in out Auto_Unit_Provider_Type);
   overriding procedure Finalize
     (Provider : in out Auto_Unit_Provider_Type);

   overriding function Get_Unit_Filename
     (Provider : Auto_Unit_Provider_Type;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String;

   overriding function Get_Unit
     (Provider    : Auto_Unit_Provider_Type;
      Context     : Analysis_Context;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit;

   function As_Key
     (Name     : Text_Type;
      Kind     : Unit_Kind;
      Provider : Auto_Unit_Provider_Type) return Symbol_Type;
   --  Given a compilation unit name and a kind (body? spec?), return a
   --  (unique) key for the unit to file mapping.

   procedure Create_Auto_Provider
     (Provider    : out Auto_Unit_Provider_Type;
      Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset);
   --  Helper for the Create_Auto_Provider functions

end Libadalang.Auto_Provider;
