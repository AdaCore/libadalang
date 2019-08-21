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

with GNATCOLL.Locks;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.GPR_Lock;
with Libadalang.Unit_Files;

package body Libadalang.Project_Provider is

   ----------------------------------
   -- Create_Project_Unit_Provider --
   ----------------------------------

   function Create_Project_Unit_Provider
     (Tree             : Prj.Project_Tree_Access;
      Project          : Prj.Project_Type := Prj.No_Project;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean := True)
      return Project_Unit_Provider
   is
      use type Prj.Project_Type;
   begin
      return Result : Project_Unit_Provider :=
        ((Tree             => Tree,
          Project          => Project,
          Env              => Env,
          Is_Project_Owner => Is_Project_Owner))
      do
         --  If no project was given, peel the aggregate project layers (if
         --  any) around Tree's root project.

         if Result.Project = Prj.No_Project then
            Result.Project := Tree.Root_Project;
            while Result.Project.Is_Aggregate_Project loop
               declare
                  Subprojects : Prj.Project_Array_Access :=
                     Result.Project.Aggregated_Projects;
                  Leave_Loop  : constant Boolean :=
                     Subprojects.all'Length /= 1;
               begin
                  if not Leave_Loop then
                     Result.Project := Subprojects.all (Subprojects.all'First);
                  end if;
                  Prj.Unchecked_Free (Subprojects);
                  exit when Leave_Loop;
               end;
            end loop;
         end if;

         if Result.Project.Is_Aggregate_Project then
            raise Prj.Invalid_Project with
               "aggregate project has too many sub-projects";
         end if;
      end return;
   end Create_Project_Unit_Provider;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String
   is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);

      Str_Name : constant String :=
        Libadalang.Unit_Files.Unit_String_Name (Name);

      File : constant Filesystem_String := Prj.File_From_Unit
        (Project   => Provider.Project,
         Unit_Name => Str_Name,
         Part      => Convert (Kind),
         Language  => "Ada");
   begin
      if File'Length = 0 then
         return "";
      end if;

      declare
         Path : constant GNATCOLL.VFS.Virtual_File :=
           Prj.Create_From_Project (Provider.Project, File).File;
      begin
         return +Path.Full_Name;
      end;
   end Get_Unit_Filename;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class
   is
      Filename : constant String := Provider.Get_Unit_Filename (Name, Kind);
   begin
      if Filename /= "" then
         return LAL.Get_From_File (Context, Filename, Charset, Reparse);
      else
         declare
            Str_Name : constant String :=
               Libadalang.Unit_Files.Unit_String_Name (Name);
            Dummy_File : constant String :=
               Libadalang.Unit_Files.File_From_Unit (Str_Name, Kind);
            Kind_Name  : constant String :=
              (case Kind is
               when Unit_Specification => "specification file",
               when Unit_Body          => "body file");
            Error      : constant String :=
               "Could not find source file for " & Str_Name & " (" & Kind_Name
               & ")";
         begin
            return LAL.Get_With_Error (Context, Dummy_File, Error, Charset);
         end;
      end if;
   end Get_Unit;

   -------------
   -- Release --
   -------------

   overriding procedure Release (Provider : in out Project_Unit_Provider)
   is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);
   begin
      Provider.Project := Prj.No_Project;
      if Provider.Is_Project_Owner then
         Prj.Unload (Provider.Tree.all);
         Prj.Free (Provider.Tree);
         Prj.Free (Provider.Env);
      end if;
      Provider.Tree := null;
      Provider.Env := null;
      Provider.Is_Project_Owner := False;
   end Release;

end Libadalang.Project_Provider;
