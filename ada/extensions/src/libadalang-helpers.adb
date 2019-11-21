------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C)      2019, AdaCore                     --
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

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Traces;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with Libadalang.Project_Provider; use Libadalang.Project_Provider;

package body Libadalang.Helpers is

   function "+" (S : String) return Unbounded_String
                 renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String renames To_String;

   package body App is
      procedure Run is

         package String_Vectors is new Ada.Containers.Vectors
           (Positive, Unbounded_String);

         Project : Project_Tree_Access;
         --  Reference to the loaded project tree, if any. Null otherwise.

         UFP : Unit_Provider_Reference;
         --  When project file handling is enabled, corresponding unit provider

         Ctx : Analysis_Context;

         Files : String_Vectors.Vector;

         Units : Unit_Vectors.Vector;

      begin
         --  Setup traces from config file
         GNATCOLL.Traces.Parse_Config_File;

         if not Args.Parser.Parse then
            return;
         end if;

         --  Handle project file
         if Length (Args.Project_File.Get) > 0 then
            declare
               Filename : constant String := +Args.Project_File.Get;
               Env      : Project_Environment_Access;
               List     : File_Array_Access;
            begin
               Project := new Project_Tree;
               Initialize (Env);

               --  Set scenario variables
               for Assoc of Args.Scenario_Vars.Get loop
                  declare
                     A        : constant String := +Assoc;
                     Eq_Index : Natural := A'First;
                  begin
                     while Eq_Index <= A'Last
                       and then A (Eq_Index) /= '=' loop
                        Eq_Index := Eq_Index + 1;
                     end loop;
                     if Eq_Index not in A'Range then
                        Put_Line ("Invalid scenario variable: -X" & A);
                        Ada.Command_Line.Set_Exit_Status
                          (Ada.Command_Line.Failure);
                        return;
                     end if;
                     Change_Environment
                       (Env.all,
                        A (A'First .. Eq_Index - 1),
                        A (Eq_Index + 1 .. A'Last));
                  end;
               end loop;

               Load (Project.all, Create (+Filename), Env);
               UFP := Create_Project_Unit_Provider_Reference
                 (Project, Project.Root_Project, Env);

               if Args.Files.Get'Length > 0 then
                  for F of Args.Files.Get loop
                     Files.Append (F);
                  end loop;
               else

                  --  If no explicit file list was passed, get a sorted list of
                  --  source files to get deterministic execution.
                  List := Project.Root_Project.Source_Files;

                  Sort (List.all);

                  for F of List.all loop
                     declare
                        FI        : constant File_Info := Project.Info (F);
                        Full_Name : Filesystem_String renames F.Full_Name.all;
                        Name      : constant String := +Full_Name;
                     begin
                        if FI.Language = "ada" then
                           Files.Append (+Name);
                        end if;
                     end;
                  end loop;
                  Unchecked_Free (List);

               end if;
            end;
         else
            --  No project passed: process the files passed explicitly
            for F of Args.Files.Get loop
               Files.Append (F);
            end loop;
         end if;

         Ctx := Create_Context
           (Charset       => +Args.Charset.Get,
            Unit_Provider => UFP);

         Process_Context_Before (Ctx, Project);

         for File of Files loop
            declare
               Unit : constant Analysis_Unit := Get_From_File (Ctx, +File);
            begin
               Process_Unit (Unit);
               Units.Append (Unit);
            end;
         end loop;

         Process_Context_After (Ctx, Project, Units);
      end Run;
   end App;

end Libadalang.Helpers;
