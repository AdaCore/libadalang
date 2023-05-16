--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Text_IO; use Ada.Text_IO;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with Libadalang.Common;            use Libadalang.Common;
with Libadalang.Config_Pragmas_Impl;
with Libadalang.GPR_Utils;         use Libadalang.GPR_Utils;
with Libadalang.Implementation;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.Config_Pragmas is

   package Impl renames Libadalang.Config_Pragmas_Impl;

   function Import_From_Project
     (Context : Analysis_Context;
      Tree    : Any_Tree;
      View    : Any_View) return Config_Pragmas_Mapping;
   --  Common implementation for the homonym public functions

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
     (Context : Analysis_Context;
      Tree    : Any_Tree;
      View    : Any_View) return Config_Pragmas_Mapping
   is
      function Fetch
        (View : Any_View; Attr : GPR_Utils.Any_Attribute) return Analysis_Unit;
      --  Return the analysis unit corresponding to the configuration pragmas
      --  file mentioned in the given project attribute, or
      --  ``No_Analysis_Unit`` if there is no such attribute.

      -----------
      -- Fetch --
      -----------

      function Fetch
        (View : Any_View; Attr : GPR_Utils.Any_Attribute) return Analysis_Unit
      is
         --  First fetch the attribute: if absent, there is no configuration
         --  pragmas file to read.

         Filename : constant String := Value (View, Attr);
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
               else Dir_Name (View) & "/" & Filename);
         begin
            return Context.Get_From_File (Full_Name);
         end;
      end Fetch;

   begin
      return Result : Config_Pragmas_Mapping do

         --  Use the global configuration pragmas file in ``Tree``, if present,
         --  to apply to all source files we'll find.

         Result.Global_Pragmas :=
           Fetch (Root (Tree),
                  Attributes.Global_Pragmas_Attribute (Tree.Kind));

         --  Iterate on all projects available from ``Tree`` to locate all
         --  local configuration pragmas files.

         declare
            Pragmas : Analysis_Unit;
            --  Local configuration pragmas file for the view processed in
            --  ``Process_View``.

            procedure Process_View (View : Any_View);
            --  Extract local configuration pragmas information from ``Self``
            --  into ``Result.Local_Pragmas``.

            procedure Associate
              (Unit_Name : String;
               Unit_Part : Any_Unit_Part;
               Filename  : String);
            --  Callback for ``Iterate_Ada_Units``. Add a ``Unit -> Pragmas``
            --  mapping to ``Result.Local_Pragmas``, with ``Unit`` being the
            --  analysis unit corresponding to ``Filename``.

            ------------------
            -- Process_View --
            ------------------

            procedure Process_View (View : Any_View) is
            begin
               --  Do not process extended projects, as for each visited
               --  project we analyze all its source files (i.e. including the
               --  sources of the project it extends).

               if Is_Extended (View) then
                  return;
               end if;

               --  Add source files to ``Result.Local_Pragmas`` iff we have
               --  local pragmas for them.

               Pragmas := Fetch
                 (View, Attributes.Local_Pragmas_Attribute (Tree.Kind));
               if Pragmas /= No_Analysis_Unit then
                  Iterate_Ada_Units
                    (Tree, View, Associate'Access, Recursive => False);
               end if;
            end Process_View;

            ---------------
            -- Associate --
            ---------------

            procedure Associate
              (Unit_Name : String;
               Unit_Part : Any_Unit_Part;
               Filename  : String)
            is
               pragma Unreferenced (Unit_Name, Unit_Part);
            begin
               Result.Local_Pragmas.Include
                 (Context.Get_From_File (Filename), Pragmas);
            end Associate;

         begin
            Iterate
              (Self    => (if View = No_View (Tree)
                           then Root (Tree)
                           else View),
               Process => Process_View'Access);
         end;
      end return;
   end Import_From_Project;

   -------------------------
   -- Import_From_Project --
   -------------------------

   function Import_From_Project
     (Context    : Analysis_Context;
      Project    : Project_Tree'Class;
      Subproject : Project_Type := No_Project) return Config_Pragmas_Mapping is
   begin
      return Import_From_Project
        (Context,
         (Kind => GPR1_Kind, GPR1_Value => Project'Unrestricted_Access),
         (Kind => GPR1_Kind, GPR1_Value => Subproject));
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

   -------------------------
   -- Import_From_Project --
   -------------------------

   function Import_From_Project
     (Context : Analysis_Context;
      Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object := GPR2.Project.View.Undefined)
      return Config_Pragmas_Mapping is
   begin
      return Import_From_Project
        (Context,
         (Kind => GPR2_Kind, GPR2_Value => Tree),
         (Kind => GPR2_Kind, GPR2_Value => View));
   end Import_From_Project;

   -------------------------
   -- Import_From_Project --
   -------------------------

   procedure Import_From_Project
     (Context : Analysis_Context;
      Tree    : GPR2.Project.Tree.Object;
      View    : GPR2.Project.View.Object := GPR2.Project.View.Undefined) is
   begin
      Set_Mapping (Context, Import_From_Project (Context, Tree, View));
   end Import_From_Project;

   ----------
   -- Dump --
   ----------

   procedure Dump (Mapping : Config_Pragmas_Mapping) is
      use Unit_Maps;
   begin
      Put_Line ("Global pragmas at: " & Mapping.Global_Pragmas.Get_Filename);
      Put_Line ("Local pragmas:");
      for Cur in Mapping.Local_Pragmas.Iterate loop
         Put_Line ("  " & Key (Cur).Get_Filename);
         Put_Line ("    -> " & Element (Cur).Get_Filename);
      end loop;
   end Dump;

end Libadalang.Config_Pragmas;
