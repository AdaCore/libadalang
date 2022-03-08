------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  This package provides the capability to automatically discover the layout
--  of source files in an Ada project, given a list of files, or a file name
--  pattern and a list of directories.
--
--  It is useful in order to easily run Libadalang on a complex project that
--  does not have its own GPR project file.

with Langkit_Support.Symbols;

private with Ada.Containers.Hashed_Maps;

with GNAT.Regexp;
with GNAT.Regpat;

with GNATCOLL.VFS;

with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Common;   use Libadalang.Common;

package Libadalang.Auto_Provider is

   use Support.Text;

   Default_Source_Filename_Pattern : constant GNAT.Regpat.Pattern_Matcher :=
     GNAT.Regpat.Compile ("\.(ad.|a|spc|bdy)$");
   Default_Source_Filename_Regexp : constant GNAT.Regexp.Regexp :=
     GNAT.Regexp.Compile (".*\.(ad.|a|spc|bdy)");
   --  Default matchers for Ada source filenames. They match most usual file
   --  extensions used for Ada sources: ``.ads``, ``.adb``, ``.ada``, ``.spc``,
   --  ``.bdy``, etc.

   function Find_Files
     (Name_Pattern : GNAT.Regpat.Pattern_Matcher :=
        Default_Source_Filename_Pattern;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access;
   --  Return the list of absolute file names for all regular files in the
   --  given ``Directories`` whose name match the given regular expression
   --  ``Name_Pattern``. The result is dynamically allocated, so the caller
   --  must free it when done with it.

   function Find_Files_Regexp
     (Name_Pattern : GNAT.Regexp.Regexp := Default_Source_Filename_Regexp;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access;
   --  Like ``Find_Files``, but works on GNAT.Regexp patterns.
   --
   --  Note: this function is not an overload, so that calls such as
   --  ``Find_Files (Directories => D);`` are not ambiguous.

   type Auto_Unit_Provider is
      new Libadalang.Analysis.Unit_Provider_Interface with private;
   --  Unit provider for a given list of files

   overriding function Get_Unit_Filename
     (Provider : Auto_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;
   --% no-document: True

   overriding function Get_Unit
     (Provider    : Auto_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class;
   --% no-document: True

   overriding procedure Release (Provider : in out Auto_Unit_Provider);
   --% no-document: True

   function Create_Auto_Provider
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset) return Auto_Unit_Provider;
   --  Return a unit provider that knows which compilation units are to be
   --  found in the given list of source files.
   --
   --  This knowledge is built trying to parse all given ``Input_Files`` as Ada
   --  source files and listing the compilation units found there. Files that
   --  cannot be parsed properly are discarded. If two compilation units are
   --  found for the same unit, the first that is found in ``Input_Files`` is
   --  taken and the other ones are discarded.
   --
   --  Source files are decoded using the given ``Charset``.
   --
   --  .. todo:: Find a way to report discarded source files/compilation units.

   function Create_Auto_Provider_Reference
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset) return Unit_Provider_Reference;
   --  Wrapper around ``Create_Auto_Provider`` as a shortcut to create a unit
   --  provider reference.
   --
   --% belongs-to: Auto_Unit_Provider

private

   use Langkit_Support.Symbols;

   use GNATCOLL.VFS;

   package CU_To_File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Symbol_Type,
      Element_Type    => Virtual_File,
      Hash            => Hash,
      Equivalent_Keys => "=");

   type Auto_Unit_Provider is new Libadalang.Analysis.Unit_Provider_Interface
   with record
      Keys    : Symbol_Table;
      Mapping : CU_To_File_Maps.Map;
   end record;

   type Auto_Unit_Provider_Access is access all Auto_Unit_Provider;

   function As_Key
     (Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Provider : Auto_Unit_Provider) return Symbol_Type;
   --  Given a compilation unit name and a kind (body? spec?), return a
   --  (unique) key for the unit to file mapping.

   procedure Create_Auto_Provider
     (Provider    : out Auto_Unit_Provider;
      Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset);
   --  Helper for the Create_Auto_Provider functions

   function Create_Auto_Provider_Reference
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset) return Unit_Provider_Reference
   is (Create_Unit_Provider_Reference
         (Create_Auto_Provider (Input_Files, Charset)));

end Libadalang.Auto_Provider;
