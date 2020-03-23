------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
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
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with Libadalang.Analysis;          use Libadalang.Analysis;
with Libadalang.Auto_Provider;     use Libadalang.Auto_Provider;
with Libadalang.GPR_Lock;
with Libadalang.Project_Provider;  use Libadalang.Project_Provider;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Implementation.C.Extensions is

   function To_C_Provider
     (Provider : Unit_Provider_Reference) return ada_unit_provider
   is (Wrap_Private_Provider (Wrap_Public_Provider (Provider)));

   function Scenario_Vars_Count
     (Scenario_Vars : System.Address) return Natural;
   --  Return the number of scenario variables in the Scenario_Vars C-style
   --  array. This counts the number of entries before the first NULL entry.

   procedure Load_Project
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr;
      Tree            : out Project_Tree_Access;
      Env             : out Project_Environment_Access);
   --  Helper to load a project file from C arguments. In case of failure,
   --  set Tree and Env to null and just return (no exception propagated).

   -------------------------
   -- Scenario_Vars_Count --
   -------------------------

   function Scenario_Vars_Count (Scenario_Vars : System.Address) return Natural
   is
      Result : Natural := 1;
      SV     : Project_Scenario_Variable_Array (Positive)
         with Import  => True,
              Address => Scenario_Vars;
   begin
      loop
         exit when SV (Result).Name = Null_Ptr;
         Result := Result + 1;
      end loop;
      return Result - 1;
   end Scenario_Vars_Count;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Project_File    : chars_ptr;
      Scenario_Vars   : System.Address;
      Target, Runtime : chars_ptr;
      Tree            : out Project_Tree_Access;
      Env             : out Project_Environment_Access)
   is
      use type System.Address;

      Dummy : GNATCOLL.Locks.Scoped_Lock (Libadalang.GPR_Lock.Lock'Access);

      PF            : constant String := Value (Project_File);
      Target_Value  : constant String :=
        (if Target = Null_Ptr then "" else Value (Target));
      Runtime_Value : constant String :=
        (if Runtime = Null_Ptr then "" else Value (Runtime));
   begin

      --  Initialize the environment (target, runtime, externals) before
      --  loading the project.

      Tree := new Project_Tree;
      Initialize (Env);
      Env.Set_Target_And_Runtime (Target_Value, Runtime_Value);
      if Scenario_Vars /= System.Null_Address then
         declare
            Vars : Project_Scenario_Variable_Array
                     (1 .. Scenario_Vars_Count (Scenario_Vars))
               with Import  => True,
                    Address => Scenario_Vars;
         begin
            for V of Vars loop
               Change_Environment (Env.all, Value (V.Name), Value (V.Value));
            end loop;
         end;
      end if;

      --  Try to load the project

      begin
         Load (Self                => Tree.all,
               Root_Project_Path   => Create (+PF),
               Env                 => Env,
               Report_Missing_Dirs => False);
      exception
         when Invalid_Project =>
            Free (Tree);
            Free (Env);
      end;
   end Load_Project;

   --------------------------------------
   -- ada_create_project_unit_provider --
   --------------------------------------

   function ada_create_project_unit_provider
     (Project_File, Project : chars_ptr;
      Scenario_Vars         : System.Address;
      Target, Runtime       : chars_ptr) return ada_unit_provider
   is
      P : constant String :=
        (if Project = Null_Ptr then "" else Value (Project));

      --  The following locals contain dynamically allocated resources. If
      --  project loading is successful, the result will own them, but in case
      --  of error, they should be free'd using the Error function below.

      Tree : Project_Tree_Access;
      Env  : Project_Environment_Access;
      Prj  : Project_Type := No_Project;

      function Error return ada_unit_provider;
      --  Helper for error handling: free allocated resources and return null

      -----------
      -- Error --
      -----------

      function Error return ada_unit_provider is
      begin
         Free (Env);
         Free (Tree);
         return ada_unit_provider (System.Null_Address);
      end Error;

   begin
      Load_Project (Project_File, Scenario_Vars, Target, Runtime, Tree, Env);
      if Env = null then
         return ada_unit_provider (System.Null_Address);
      end if;

      if P /= "" then
         --  A specific project was requested: try to build one unit provider
         --  just for it. Lookup the project by name, and then if not found, by
         --  path.
         Prj := Tree.Project_From_Name (P);
         if Prj = No_Project then
            Prj := Tree.Project_From_Path (Create (+P));
         end if;
         if Prj = No_Project then
            return Error;
         end if;
      end if;

      begin
         return To_C_Provider
           (Create_Project_Unit_Provider (Tree, Prj, Env, True));
      exception
         when Invalid_Project =>
            return Error;
      end;
   end ada_create_project_unit_provider;

   ------------------------------
   -- ada_create_auto_provider --
   ------------------------------

   function ada_create_auto_provider
     (Input_Files : System.Address;
      Charset     : chars_ptr)
      return ada_unit_provider
   is
      type C_String_Array is array (Positive) of chars_ptr
         with Convention => C;

      Files_Count       : Natural := 0;
      Input_Files_Array : C_String_Array with
         Import  => True,
         Address => Input_Files;

      Actual_Charset : constant String :=
        (if Charset = Null_Ptr then Default_Charset else Value (Charset));
   begin
      while Input_Files_Array (Files_Count + 1) /= Null_Ptr loop
         Files_Count := Files_Count + 1;
      end loop;

      declare
         --  Allocate the array of filenames on the heap, as it may be too
         --  large for the stack.

         Files : File_Array_Access := new File_Array (1 .. Files_Count);
      begin
         for I in Files'Range loop
            Files (I) := Create (+Value (Input_Files_Array (I)));
         end loop;

         return Provider : constant ada_unit_provider := To_C_Provider
           (Create_Auto_Provider_Reference (Files.all, Actual_Charset))
         do
            Unchecked_Free (Files);
         end return;
      end;
   end ada_create_auto_provider;

end Libadalang.Implementation.C.Extensions;
