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

with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Interfaces; use Interfaces;

with GNATCOLL.Locks;
with GNATCOLL.Projects;
with GNATCOLL.VFS; use GNATCOLL.VFS;

with Libadalang.GPR_Lock;
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

   --------------------
   -- Unit_Text_Name --
   --------------------

   function Unit_Text_Name (N : LAL.Name) return Text_Type is
   begin
      if N.Kind = Ada_Identifier then
         return N.Text;

      elsif N.Kind = Ada_Dotted_Name then
         declare
            DN : constant LAL.Dotted_Name := N.As_Dotted_Name;
         begin
            if DN.F_Prefix.Kind in Ada_Name
               and then DN.F_Suffix.Kind = Ada_Identifier
            then
               return (Unit_Text_Name (DN.F_Prefix.As_Name)
                       & "."
                       & Unit_Text_Name (DN.F_Suffix.As_Name));
            end if;
         end;
      end if;

      raise Property_Error with "invalid AST node for unit name";
   end Unit_Text_Name;

   Dot        : constant := Character'Pos ('.');
   Dash       : constant := Character'Pos ('-');
   Underscore : constant := Character'Pos ('_');
   Zero       : constant := Character'Pos ('0');
   Nine       : constant := Character'Pos ('9');
   Lower_A    : constant := Character'Pos ('a');
   Upper_A    : constant := Character'Pos ('A');
   Lower_Z    : constant := Character'Pos ('z');
   Upper_Z    : constant := Character'Pos ('Z');

   ----------------------
   -- Unit_String_Name --
   ----------------------

   function Unit_String_Name (Name : Text_Type) return String is
      Result : String (Name'Range);
   begin

      --  Make Name lower case. Only allow ASCII.

      for I in Name'Range loop
         declare
            C  : constant Wide_Wide_Character := Name (I);
            CN : constant Unsigned_32 := Wide_Wide_Character'Pos (C);
         begin
            case CN is
               when Dot
                    | Underscore
                    | Dash
                    | Zero .. Nine
                    | Upper_A .. Upper_Z
                    | Lower_A .. Lower_Z =>
                  Result (I) := Ada.Strings.Maps.Value
                    (Ada.Strings.Maps.Constants.Lower_Case_Map,
                     Character'Val (CN));
               when others =>
                  raise Property_Error with
                    ("unhandled unit name: character "
                     & Image (T => (1 => C), With_Quotes => True)
                     & " not supported");
            end case;
         end;
      end loop;

      return Result;
   end Unit_String_Name;

   --------------------
   -- File_From_Unit --
   --------------------

   function File_From_Unit
     (Name : String; Kind : Analysis_Unit_Kind) return String
   is
      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);
   begin
      return +GNATCOLL.Projects.File_From_Unit
        (GNATCOLL.Projects.No_Project,
         Name,
         Libadalang.Project_Provider.Convert (Kind),
         "ada");
   end File_From_Unit;

end Libadalang.Unit_Files;
