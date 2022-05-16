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

with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Wide_Wide_Characters.Handling;

with GNAT.Task_Lock;

with GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Project_Provider;

package body Libadalang.Unit_Files is

   ----------------------
   -- Default_Provider --
   ----------------------

   function Default_Provider return LAL.Unit_Provider_Reference is
      use GNATCOLL.Projects;
      use Libadalang.Project_Provider;

      Env     : Project_Environment_Access;
      Project : constant Project_Tree_Access := new Project_Tree;
   begin
      Initialize (Env);
      Load_Empty_Project (Project.all, Env);
      Project.Root_Project.Delete_Attribute (Source_Dirs_Attribute);
      Project.Root_Project.Delete_Attribute (Languages_Attribute);
      Project.Recompute_View;

      return Create_Project_Unit_Provider
        (Project, Project.Root_Project, Env, True);
   end Default_Provider;

   ----------------------
   -- Unit_String_Name --
   ----------------------

   function Unit_String_Name (Name : Text_Type) return String is
      Result : Unbounded_String;
   begin
      --  Make Name lower case. Process ASCII separately to keep the process of
      --  lowering the case efficient in the common case.
      --
      --  TODO??? This assumes that the file system uses UTF-8. It's not clear
      --  where this information should come from.

      for I in Name'Range loop
         declare
            C : constant Wide_Wide_Character := Name (I);
         begin
            case C is
               when Wide_Wide_Character'Val (0)
                 .. Wide_Wide_Character'Val (255)
               =>
                  Append
                    (Result,
                     Ada.Strings.Maps.Value
                       (Ada.Strings.Maps.Constants.Lower_Case_Map,
                        Character'Val (Wide_Wide_Character'Pos (C))));
               when others =>
                  declare
                     Lower : constant Wide_Wide_Character :=
                        Ada.Wide_Wide_Characters.Handling.To_Lower (C);
                  begin
                     Append (Result, To_UTF8 ((1 => Lower)));
                  end;
            end case;
         end;
      end loop;

      return To_String (Result);
   end Unit_String_Name;

   --------------------
   -- File_From_Unit --
   --------------------

   function File_From_Unit
     (Name : Text_Type; Kind : Analysis_Unit_Kind) return String is
   begin
      GNAT.Task_Lock.Lock;

      return Result : constant String := +GNATCOLL.Projects.File_From_Unit
        (GNATCOLL.Projects.No_Project,
         Unit_String_Name (Name),
         Libadalang.Project_Provider.Convert (Kind),
         "ada")
      do
         GNAT.Task_Lock.Unlock;
      end return;

   exception
      when others =>
         GNAT.Task_Lock.Unlock;
         raise;
   end File_From_Unit;

end Libadalang.Unit_Files;
