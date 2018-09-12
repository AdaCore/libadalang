------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2018, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.   See the  GNU  --
-- General  Public  License for more details.   You should have received a  --
-- copy of the GNU General Public License  distributed with this software;  --
-- see  file  COPYING3.  If not, go to  http://www.gnu.org/licenses  for a  --
-- complete copy of the license.                                            --
------------------------------------------------------------------------------

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.Unit_Files.Default;

package body Libadalang.Unit_Files.Projects is

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Dummy : Scoped_Lock (GPR_Lock'Access);

      Str_Name : constant String :=
        Libadalang.Unit_Files.Default.Unit_String_Name (Name);

      File : constant Filesystem_String := Prj.File_From_Unit
        (Project   => Prj.Root_Project (Provider.Project.all),
         Unit_Name => Str_Name,
         Part      => Convert (Kind),
         Language  => "Ada");
   begin
      if File'Length = 0 then
         return "";
      end if;

      declare
         Path : constant GNATCOLL.VFS.Virtual_File :=
           Prj.Create (Provider.Project.all, File);
      begin
         return +Full_Name (Path);
      end;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LP.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LP.Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return LP.Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Str_Name : constant String :=
               Libadalang.Unit_Files.Default.Unit_String_Name (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.Default.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return LP.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Project_Unit_Provider)
   is
      Dummy : Scoped_Lock (GPR_Lock'Access);
   begin
      if Provider.Is_Project_Owner then
         Prj.Unload (Provider.Project.all);
         Prj.Free (Provider.Project);
         Prj.Free (Provider.Env);
      end if;
      Provider.Project := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Release;

end Libadalang.Unit_Files.Projects;
