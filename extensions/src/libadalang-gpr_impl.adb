--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with GNATCOLL.Strings; use GNATCOLL.Strings;
with GPR2.Path_Name;

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Config_Pragmas;    use Libadalang.Config_Pragmas;
with Libadalang.GPR_Utils;         use Libadalang.GPR_Utils;
with Libadalang.Implementation.Extensions;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Libadalang.Project_Provider;  use Libadalang.Project_Provider;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;
with Libadalang.Target_Info;       use Libadalang.Target_Info;

package body Libadalang.GPR_Impl is

   procedure Extract_Target_Info
     (Tree            : GPR2.Project.Tree.Object;
      View            : GPR2.Project.View.Object :=
        GPR2.Project.View.Undefined;
      Target_Info     : out Target_Information;
      Has_Target_Info : out Boolean);
   --  Look for target information in the given tree/project. If found, set
   --  ``Has_Target_Info`` to True and ``Target_Info`` to the relevant target
   --  information. Otherwise, set ``Has_Target_Info`` to False.

   -------------------------
   -- Extract_Target_Info --
   -------------------------

   procedure Extract_Target_Info
     (Tree            : GPR2.Project.Tree.Object;
      View            : GPR2.Project.View.Object :=
        GPR2.Project.View.Undefined;
      Target_Info     : out Target_Information;
      Has_Target_Info : out Boolean)
   is
      --  If we can find a -gnateT compilation switch, load target information
      --  from the file it designates.

      Prefix   : constant String := "-gnateT=";
      Filename : XString;

      procedure Process_Switch (View : Any_View; Switch : XString);
      --  If ``Switch`` is ``-gnateT``, set ``Filename`` to its argument

      procedure Process_Switch (View : Any_View; Switch : XString) is
         pragma Unreferenced (View);
      begin
         if Switch.Starts_With (Prefix) then
            Filename := Switch.Slice (Prefix'Length + 1, Switch.Length);
         end if;
      end Process_Switch;

   begin
      Iterate_Ada_Compiler_Switches
        (Tree    => (Kind => GPR2_Kind, GPR2_Value => Tree),
         View    => (Kind => GPR2_Kind, GPR2_Value => View),
         Process => Process_Switch'Access);
      if not Filename.Is_Empty then
         Target_Info := Load (Filename.To_String);
         Has_Target_Info := True;
         return;
      end if;

      --  If there is a runtime project and we can find an
      --  "ada_target_properties" file in it, use it to set the right target
      --  information.

      if Tree.Has_Runtime_Project then
         declare
            Filename : constant GPR2.Path_Name.Object :=
              Tree.Runtime_Project.Dir_Name.Compose ("ada_target_properties");
         begin
            if Filename.Exists then
               Target_Info := Load (String (Filename.Name));
               Has_Target_Info := True;
               return;
            end if;
         end;
      end if;

      --  We found no relevant target information: make sure OUT arguments are
      --  initialized.

      Target_Info := Placeholder_Target_Info;
      Has_Target_Info := False;
   end Extract_Target_Info;

   -------------------------------------
   -- Initialize_Context_From_Project --
   -------------------------------------

   procedure Initialize_Context_From_Project
     (Context          : Internal_Context;
      Tree             : GNATCOLL.Projects.Project_Tree_Access;
      Project          : GNATCOLL.Projects.Project_Type;
      Env              : GNATCOLL.Projects.Project_Environment_Access;
      Is_Project_Owner : Boolean;
      Event_Handler    : Internal_Event_Handler_Access;
      With_Trivia      : Boolean;
      Tab_Stop         : Positive)
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree.all, Project, Default_Config, File_Configs);

      declare
         UFP     : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider (Tree, Project, Env, Is_Project_Owner);
         FR      : constant File_Reader_Reference :=
           Create_Preprocessor (Default_Config, File_Configs);
         Charset : constant String :=
           Default_Charset_From_Project (Tree.all, Project);

         UFP_Int : Internal_Unit_Provider_Access := Wrap_Public_Provider (UFP);
         FR_Int  : Internal_File_Reader_Access := Wrap_Public_File_Reader (FR);
      begin
         Initialize_Context
           (Context       => Context,
            Charset       => Charset,
            File_Reader   => FR_Int,
            Unit_Provider => UFP_Int,
            Event_Handler => Event_Handler,
            With_Trivia   => With_Trivia,
            Tab_Stop      => Tab_Stop);

         --  Now that ``Initialize_Context`` has created ownership share for
         --  its own copies of ``UFP_Int`` and ``FR_Int``, we can release ours.

         Dec_Ref (UFP_Int);
         Dec_Ref (FR_Int);
      end;

      declare
         Ctx : constant Analysis_Context := Wrap_Context.all (Context);
      begin
         Import_From_Project (Ctx, Tree.all, Project);
      end;
   end Initialize_Context_From_Project;

   -------------------------------------
   -- Initialize_Context_From_Project --
   -------------------------------------

   procedure Initialize_Context_From_Project
     (Context       : Internal_Context;
      Tree          : GPR2.Project.Tree.Object;
      Project       : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Event_Handler : Internal_Event_Handler_Access;
      With_Trivia   : Boolean;
      Tab_Stop      : Positive)
   is
      Default_Config : File_Config;
      File_Configs   : File_Config_Maps.Map;

      Has_Target_Info : Boolean := False;
      Target_Info     : Target_Information;
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree, Project, Default_Config, File_Configs);

      --  If we can find a -gnateT compilation switch, load target information
      --  from the file it designates.

      Extract_Target_Info (Tree, Project, Target_Info, Has_Target_Info);

      declare
         UFP     : constant Unit_Provider_Reference :=
           Create_Project_Unit_Provider (Tree, Project);
         FR      : constant File_Reader_Reference :=
           Create_Preprocessor (Default_Config, File_Configs);
         Charset : constant String :=
           Default_Charset_From_Project (Tree, Project);

         UFP_Int : Internal_Unit_Provider_Access := Wrap_Public_Provider (UFP);
         FR_Int  : Internal_File_Reader_Access := Wrap_Public_File_Reader (FR);
      begin
         Initialize_Context
           (Context       => Context,
            Charset       => Charset,
            File_Reader   => FR_Int,
            Unit_Provider => UFP_Int,
            Event_Handler => Event_Handler,
            With_Trivia   => With_Trivia,
            Tab_Stop      => Tab_Stop);

         --  Now that ``Initialize_Context`` has created ownership share for
         --  its own copies of ``UFP_Int`` and ``FR_Int``, we can release ours.

         Dec_Ref (UFP_Int);
         Dec_Ref (FR_Int);
      end;

      declare
         Ctx : constant Analysis_Context := Wrap_Context.all (Context);
      begin
         Import_From_Project (Ctx, Tree, Project);
      end;

      if Has_Target_Info then
         Libadalang.Implementation.Extensions.Set_Target_Information
           (Context, Target_Info);
      end if;
   end Initialize_Context_From_Project;

end Libadalang.GPR_Impl;
