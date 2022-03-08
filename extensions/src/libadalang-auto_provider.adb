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

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded;
with Ada.Wide_Wide_Characters.Handling;

with Libadalang.Unit_Files;

package body Libadalang.Auto_Provider is

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Filter       : not null access function (Name : String) return Boolean;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access;
   --  Common implementation for the two public ``Find_Files`` functions:
   --  return the list of files in ``Directories`` for which ``Filter`` (when
   --  called on the file base name) returns True.

   procedure Add_Entry
     (Provider : in out Auto_Unit_Provider;
      Filename : Virtual_File;
      CU       : Compilation_Unit);
   --  Add a CU -> Filename entry to Provider.Mapping

   ---------------
   -- Add_Entry --
   ---------------

   procedure Add_Entry
     (Provider : in out Auto_Unit_Provider;
      Filename : Virtual_File;
      CU       : Compilation_Unit)
   is
      use Ada.Strings.Wide_Wide_Unbounded;

      FQN  : constant Unbounded_Text_Type_Array :=
        CU.P_Syntactic_Fully_Qualified_Name;
      Kind : constant Analysis_Unit_Kind := CU.P_Unit_Kind;
      Name : Unbounded_Wide_Wide_String;
   begin
      for I in FQN'Range loop
         if I > FQN'First then
            Append (Name, '.');
         end if;
         Append (Name, To_Text (FQN (I)));
      end loop;

      declare
         Key      : constant Symbol_Type := As_Key (To_Wide_Wide_String (Name),
                                                    Kind, Provider);
         Dummy_Cur : CU_To_File_Maps.Cursor;
         Inserted  : Boolean;
      begin
         Provider.Mapping.Insert (Key, Filename, Dummy_Cur, Inserted);

         --  TODO??? Somehow report duplicate entries
         pragma Unreferenced (Inserted);
      end;
   end Add_Entry;

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Filter       : not null access function (Name : String) return Boolean;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      package File_Vectors is new Ada.Containers.Vectors
        (Positive, Virtual_File);
      use File_Vectors;

      Result : Vector;
   begin
      for D of Directories loop
         declare
            Files : File_Array_Access := Read_Dir_Recursive
               (D, Filter => Files_Only);
         begin
            for F of Files.all loop
               if Filter.all (+F.Base_Name) then
                  Result.Append (F);
               end if;
            end loop;
            Unchecked_Free (Files);
         end;
      end loop;

      return R : constant File_Array_Access :=
         new File_Array (1 .. Natural (Result.Length))
      do
         for Cur in Result.Iterate loop
            R (To_Index (Cur)) := Element (Cur);
         end loop;
      end return;
   end Find_Files;

   ----------------
   -- Find_Files --
   ----------------

   function Find_Files
     (Name_Pattern : GNAT.Regpat.Pattern_Matcher :=
        Default_Source_Filename_Pattern;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      function Filter (Name : String) return Boolean
      is (GNAT.Regpat.Match (Name_Pattern, Name));
   begin
      return Find_Files (Filter'Access, Directories);
   end Find_Files;

   -----------------------
   -- Find_Files_Regexp --
   -----------------------

   function Find_Files_Regexp
     (Name_Pattern : GNAT.Regexp.Regexp := Default_Source_Filename_Regexp;
      Directories  : GNATCOLL.VFS.File_Array)
      return GNATCOLL.VFS.File_Array_Access
   is
      function Filter (Name : String) return Boolean
      is (GNAT.Regexp.Match (Name, Name_Pattern));
   begin
      return Find_Files (Filter'Access, Directories);
   end Find_Files_Regexp;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Auto_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      use CU_To_File_Maps;

      Cur : constant Cursor := Provider.Mapping.Find
        (As_Key (Name, Kind, Provider));
   begin
      if Cur = No_Element then
         return "";
      else
         return +Full_Name (Element (Cur));
      end if;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Auto_Unit_Provider;
      Context     : Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Name, Kind);
            Kind_Name  : constant Text_Type :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant Text_Type :=
               "Could not find source file for " & Name & " (" & Kind_Name
               & ")";
         begin
            return Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Auto_Unit_Provider) is
   begin
      Provider.Mapping.Clear;
      Destroy (Provider.Keys);
   end Release;

   --------------------------
   -- Create_Auto_Provider --
   --------------------------

   procedure Create_Auto_Provider
     (Provider    : out Auto_Unit_Provider;
      Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset)
   is
      Context : constant Analysis_Context := Create_Context (Charset);
   begin
      --  Go through every input file and try to parse them
      for File of Input_Files loop
         declare
            F    : constant String := +File.Full_Name;
            Unit : constant Analysis_Unit :=
               Get_From_File (Context, F, Reparse => True);
            R    : constant Ada_Node := Root (Unit);
         begin
            if not Has_Diagnostics (Unit) then
               --  If parsing went fine, add the compilation units File
               --  contains to our internal mapping.
               --
               --  TODO??? Somehow report parsing errors.
               case Unit_Files.Root_Nodes (R.Kind) is
                  when Ada_Compilation_Unit =>
                     Add_Entry (Provider, File, R.As_Compilation_Unit);
                  when Ada_Compilation_Unit_List =>
                     for CU of R.Children loop
                        Add_Entry (Provider, File, CU.As_Compilation_Unit);
                     end loop;

                  when Ada_Pragma_Node_List =>
                     --  This could be a configuration pragma file, or a body
                     --  that contains just "pragma No_Body;". In any case,
                     --  there is no entry to register here.
                     null;
               end case;
            end if;
         end;

      end loop;
   end Create_Auto_Provider;

   --------------------------
   -- Create_Auto_Provider --
   --------------------------

   function Create_Auto_Provider
     (Input_Files : GNATCOLL.VFS.File_Array;
      Charset     : String := Default_Charset) return Auto_Unit_Provider is
   begin
      return Provider : Auto_Unit_Provider do
         Provider.Keys := Create_Symbol_Table;
         Create_Auto_Provider (Provider, Input_Files, Charset);
      end return;
   end Create_Auto_Provider;

   ------------
   -- As_Key --
   ------------

   function As_Key
     (Name     : Text_Type;
      Kind     : Analysis_Unit_Kind;
      Provider : Auto_Unit_Provider) return Symbol_Type
   is
      Canon_Name  : constant Text_Type :=
         Ada.Wide_Wide_Characters.Handling.To_Lower (Name);
      Kind_Letter : constant Wide_Wide_Character :=
        (case Kind is
         when Unit_Specification => 's',
         when Unit_Body          => 'b');
   begin
      return Get_Symbol
        (Provider.Keys, Find (Provider.Keys, Kind_Letter & ':' & Canon_Name));
   end As_Key;

end Libadalang.Auto_Provider;
