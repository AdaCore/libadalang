--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Directories;         use Ada.Directories;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Wide_Wide_Characters.Handling;

with GPR2.Build.Compilation_Unit;
with GPR2.Build.Source.Sets;
with GPR2.Containers;
with GPR2.Options;
with GPR2.Path_Name;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree.View_Builder;
with GPR2.Project.View;
with GPR2.Reporter.Console;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Project_Provider;

package body Libadalang.Unit_Files is

   ----------------------------
   --  Default unit provider --
   ----------------------------

   --  By default, Libadalang uses a unit provider that gives access to the
   --  native runtime (reached through LibGPR2, see the Default_Provider
   --  function), and assumes that other units are stored as individual source
   --  files in the current directory, following the GNAT naming convention.

   Spec_Prefix : constant String := "s" & ASCII.NUL;
   Body_Prefix : constant String := "b" & ASCII.NUL;

   package Unit_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => GPR2.Build.Compilation_Unit.Unit_Location,
      Equivalent_Keys => "=",
      "="             => GPR2.Build.Compilation_Unit."=",
      Hash            => Hash);
   --  Maps units to the corresponding source filenames.
   --
   --  For each entry, the key is Spec_Prefix or Body_Prefix (depending on
   --  whether the unit is a spec or a body/separate) concatenated to the
   --  lower-case unit name.

   type Default_Unit_Provider is new LAL.Unit_Provider_Interface with record
      Runtime_Units : Unit_Maps.Map;
   end record;

   overriding function Get_Unit_Filename
     (Provider : Default_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String;

   overriding procedure Get_Unit_Location
     (Provider       : Default_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Unbounded_String;
      PLE_Root_Index : in out Natural);

   overriding function Get_Unit
     (Provider    : Default_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class;

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Default_Unit_Provider;
      Context        : LAL.Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out LAL.Analysis_Unit'Class;
      PLE_Root_Index : in out Natural);

   overriding procedure Release (Provider : in out Default_Unit_Provider)
   is null;

   -----------------------
   -- Get_Unit_Filename --
   -----------------------

   overriding function Get_Unit_Filename
     (Provider : Default_Unit_Provider;
      Name     : Text_Type;
      Kind     : Analysis_Unit_Kind) return String is
   begin
      --  Get_Unit_Location is supposed to handle all cases, so this should be
      --  dead code.

      return (raise Program_Error);
   end Get_Unit_Filename;

   -----------------------
   -- Get_Unit_Location --
   -----------------------

   overriding procedure Get_Unit_Location
     (Provider       : Default_Unit_Provider;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Filename       : in out Unbounded_String;
      PLE_Root_Index : in out Natural)
   is
      use Unit_Maps;
      use type GPR2.Unit_Index;
      Key : Unbounded_String;
      Cur : Unit_Maps.Cursor;
   begin
      --  First look in runtime units

      if Kind = Unit_Specification then
         Append (Key, Spec_Prefix);
      else
         Append (Key, Body_Prefix);
      end if;
      Append (Key, Unit_String_Name (Name));
      Cur := Provider.Runtime_Units.Find (Key);
      if Has_Element (Cur) then
         declare
            Loc : GPR2.Build.Compilation_Unit.Unit_Location :=
              Element (Cur);
         begin
            --  GPR2 sets the CU index to 0 when there is no "at N" clause in
            --  the project file. This is equivalont to "at 1", which is what
            --  we need here since PLE_Root_Index is a Positive.

            Filename := To_Unbounded_String (String (Loc.Source.Value));
            PLE_Root_Index :=
              (if Loc.Index = 0 then 1 else Positive (Loc.Index));
            return;
         end;
      end if;

      --  If not successful, redirect to the current working directory assuming
      --  GNAT naming convention.

      PLE_Root_Index := 1;
      declare
         Str_Filename : constant String := File_From_Unit (Name, Kind);
      begin
         Filename :=
           (if Exists (Str_Filename)
            then To_Unbounded_String (Str_Filename)
            else Null_Unbounded_String);
      end;
   end Get_Unit_Location;

   --------------
   -- Get_Unit --
   --------------

   overriding function Get_Unit
     (Provider    : Default_Unit_Provider;
      Context     : LAL.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Analysis_Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LAL.Analysis_Unit'Class
   is
      --  Get_Unit_And_PLE_Root is supposed to handle all cases, so this should
      --  be dead code.

      pragma Unreferenced (Provider, Context, Name, Kind, Charset, Reparse);
   begin
      return (raise Program_Error);
   end Get_Unit;

   ---------------------------
   -- Get_Unit_And_PLE_Root --
   ---------------------------

   overriding procedure Get_Unit_And_PLE_Root
     (Provider       : Default_Unit_Provider;
      Context        : LAL.Analysis_Context'Class;
      Name           : Text_Type;
      Kind           : Analysis_Unit_Kind;
      Charset        : String := "";
      Reparse        : Boolean := False;
      Unit           : in out LAL.Analysis_Unit'Class;
      PLE_Root_Index : in out Natural)
   is
      function "+" (Kind : Analysis_Unit_Kind) return String
      is (case Kind is
          when Unit_Specification => "specification",
          when Unit_Body          => "body");
      Filename : Unbounded_String;
   begin
      --  Query the unit location. In case of lookup failure, return a dummy
      --  unit.

      Provider.Get_Unit_Location (Name, Kind, Filename, PLE_Root_Index);
      if Filename = "" then
         Unit := LAL.Analysis_Unit'Class
           (Context.Get_With_Error
              (File_From_Unit (Name, Kind),
               "Could not find source file for "
               & Name
               & " ("
               & To_Text
                   (case Kind is
                    when Unit_Specification => "specification",
                    when Unit_Body          => "body")
               & " file)",
               Charset));
         PLE_Root_Index := 1;
      else
         Unit := LAL.Analysis_Unit'Class
           (Context.Get_From_File (To_String (Filename), Charset, Reparse));
      end if;
   end Get_Unit_And_PLE_Root;

   ----------------------
   -- Default_Provider --
   ----------------------

   function Default_Provider return LAL.Unit_Provider_Reference is
      use type GPR2.Unit_Kind;

      --  Create a synthetic project tree that has no source, but still has
      --  access to the native Ada runtime.

      package PRA renames GPR2.Project.Registry.Attribute;
      package View_Builder renames GPR2.Project.Tree.View_Builder;

      Languages  : GPR2.Containers.Value_List;
      No_Sources : GPR2.Containers.Value_List;
      Root       : View_Builder.Object :=
        View_Builder.Create
          (Project_Dir => GPR2.Path_Name.Create_Directory ("."),
           Name        => "empty");
      Tree       : GPR2.Project.Tree.Object;
      Result     : Default_Unit_Provider;
   begin
      Languages.Append ("Ada");
      Root.Set_Attribute (PRA.Languages, Languages);
      Root.Set_Attribute (PRA.Source_Files, No_Sources);

      if not Tree.Load_Virtual_View
        (Root_Project     => Root,
         Options          => GPR2.Options.Empty_Options,
         With_Runtime     => True,
         Absent_Dir_Error => GPR2.No_Error,
         Reporter         => GPR2.Reporter.Console.Create
                               (GPR2.Reporter.Quiet))
      then
         raise Program_Error;
      end if;
      Tree.Update_Sources
        (Option         => GPR2.Sources_Units,
         No_Error       => True,
         Matching_Units => False);

      --  Build a map of all runtime units so that the provider can look them
      --  up easily.

      if Tree.Has_Runtime_Project then
         for Source of Tree.Runtime_Project.Sources loop
            if Source.Has_Units then
               for Unit of Source.Units loop
                  if Unit.Kind /= GPR2.S_No_Body then
                     declare
                        use GPR2.Build.Compilation_Unit;
                        Key : Unbounded_String :=
                          To_Unbounded_String
                            (if Unit.Kind = GPR2.S_Spec
                             then Spec_Prefix
                             else Body_Prefix);
                        Element : constant Unit_Location :=
                          (GPR2.Project.View.Undefined,
                           Source.Path_Name,
                           Unit.Index);
                     begin
                        Append (Key, To_Lower (String (Unit.Full_Name)));
                        Result.Runtime_Units.Include (Key, Element);
                     end;
                  end if;
               end loop;
            end if;
         end loop;
      end if;

      return Libadalang.Analysis.Create_Unit_Provider_Reference (Result);
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
                 .. Wide_Wide_Character'Val (127)
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
                     Append (Result, To_UTF8 ([Lower]));
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
     (Name : Text_Type; Kind : Analysis_Unit_Kind) return String
   is
      Prefix : String := Unit_String_Name (Name);
      Ext    : constant String :=
        (case Kind is
           when Unit_Specification => ".ads",
           when Unit_Body          => ".adb");
   begin
      for C of Prefix loop
         if C = '.' then
            C := '-';
         end if;
      end loop;
      return Prefix & Ext;
   end File_From_Unit;

end Libadalang.Unit_Files;
