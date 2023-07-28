--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;

with Libadalang.Config_Pragmas;    use Libadalang.Config_Pragmas;
with Libadalang.Preprocessing;     use Libadalang.Preprocessing;
with Libadalang.Project_Provider;  use Libadalang.Project_Provider;
with Libadalang.Public_Converters; use Libadalang.Public_Converters;

package body Libadalang.GPR_Impl is

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
   begin
      Extract_Preprocessor_Data_From_Project
        (Tree, Project, Default_Config, File_Configs);

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
   end Initialize_Context_From_Project;

end Libadalang.GPR_Impl;
