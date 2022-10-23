--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.VFS; use GNATCOLL.VFS;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Libadalang.Common;            use Libadalang.Common;
with Libadalang.Config_Pragmas_Impl;
with Libadalang.Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Config_Pragmas is

   package Impl renames Libadalang.Config_Pragmas_Impl;

   -----------------
   -- Set_Mapping --
   -----------------

   procedure Set_Mapping
     (Context : Analysis_Context; Mapping : Config_Pragmas_Mapping)
   is
      use Libadalang.Implementation;
      use Unit_Maps;

      C      : constant Internal_Context := Unwrap_Context (Context);
      Global : constant Internal_Unit := Unwrap_Unit (Mapping.Global_Pragmas);
   begin
      --  Validate all arguments first, so that this procedure is atomic:
      --  either it fails (and changes nothing), either it completes.

      if C = null then
         raise Precondition_Failure with "null context";
      end if;

      if Global /= null and then Global.Context /= C then
         raise Precondition_Failure with "foreign unit";
      end if;

      for Cur in Mapping.Local_Pragmas.Iterate loop
         declare
            K : constant Internal_Unit := Unwrap_Unit (Key (Cur));
            V : constant Internal_Unit := Unwrap_Unit (Element (Cur));
         begin
            if K = null then
               raise Precondition_Failure with "null unit key";
            elsif V = null then
               raise Precondition_Failure with "null unit value";
            elsif K.Context /= C or else V.Context /= C then
               raise Precondition_Failure with "foreign unit";
            end if;
         end;
      end loop;

      --  Do the assignment

      C.Config_Pragmas.Local_Pragmas.Clear;
      for Cur in Mapping.Local_Pragmas.Iterate loop
         declare
            K : constant Internal_Unit := Unwrap_Unit (Key (Cur));
            V : constant Internal_Unit := Unwrap_Unit (Element (Cur));
         begin
            C.Config_Pragmas.Local_Pragmas.Include
              (Impl.Internal_Unit (K), Impl.Internal_Unit (V));
         end;
      end loop;

      C.Config_Pragmas.Global_Pragmas :=
        Impl.Internal_Unit (Unwrap_Unit (Mapping.Global_Pragmas));

      --  Invalidate caches, as this new assignment may change name resolution

      Invalidate_Caches (C, Invalidate_Envs => True);

      --  Users can now use the properties to query configuration pragmas
      C.Config_Pragmas_Set := True;
   end Set_Mapping;

   -------------------------
   -- Import_From_Project --
   -------------------------

   function Import_From_Project
     (Context    : Analysis_Context;
      Project    : Project_Tree'Class;
      Subproject : Project_Type := No_Project) return Config_Pragmas_Mapping
   is

      function Fetch
        (Subproject : Project_Type;
         Attr       : Attribute_Pkg_String) return Analysis_Unit;
      --  Return the analysis unit corresponding to the configuration pragmas
      --  file mentioned in the given project attribute, or
      --  ``No_Analysis_Unit`` if there is no such attribute.

      -----------
      -- Fetch --
      -----------

      function Fetch
        (Subproject : Project_Type;
         Attr       : Attribute_Pkg_String) return Analysis_Unit
      is
         --  First fetch the attribute: if absent, there is no configuration
         --  pragmas file to read.

         Filename : constant String :=
           Subproject.Attribute_Value (Attr, Use_Extended => False);
      begin
         if Filename = "" then
            return No_Analysis_Unit;
         end if;

         --  The attribute specifies a path that is either absolute, or
         --  relative to the project directory: get an absolute name in the
         --  latter case.

         declare
            Full_Name : constant String :=
              (if Is_Absolute_Path (Filename)
               then Filename
               else (+Subproject.Project_Path.Dir_Name) & "/" & Filename);
         begin
            return Context.Get_From_File (Full_Name);
         end;
      end Fetch;

      It      : Project_Iterator;
      P       : Project_Type;
      Pragmas : Analysis_Unit;
      Files   : File_Array_Access;

   begin
      return Result : Config_Pragmas_Mapping do

         --  Use the global configuration pragmas file in ``Project``, if
         --  present, to apply to all source files we'll find.

         Result.Global_Pragmas :=
           Fetch (Project.Root_Project, Global_Pragmas_Attribute);

         --  Iterate on all projects available from ``Project`` to locate all
         --  local configuration pragmas files.

         It :=
           (if Subproject = No_Project
            then Project.Root_Project.Start
            else Subproject.Start);
         loop
            P := Current (It);
            exit when P = No_Project;

            --  Do not process extended projects, as for each visited project
            --  we analyze all its source files (i.e. including the sources of
            --  the project it extends).

            if Extending_Project (P) = No_Project then

               --  Add source files to ``Result.Local_Pragmas`` iff we have
               --  local pragmas for them.

               Pragmas := Fetch (P, Local_Pragmas_Attribute);
               if Pragmas /= No_Analysis_Unit then
                  Files := P.Extended_Projects_Source_Files;
                  for F of Files.all loop
                     Result.Local_Pragmas.Include
                       (Context.Get_From_File (+F.Full_Name), Pragmas);
                  end loop;
                  Unchecked_Free (Files);
               end if;
            end if;

            Next (It);
         end loop;

      end return;
   end Import_From_Project;

   -------------------------
   -- Import_From_Project --
   -------------------------

   procedure Import_From_Project
     (Context    : Analysis_Context;
      Project    : Project_Tree'Class;
      Subproject : Project_Type := No_Project) is
   begin
      Set_Mapping
        (Context, Import_From_Project (Context, Project, Subproject));
   end Import_From_Project;

end Libadalang.Config_Pragmas;
