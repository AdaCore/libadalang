--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helpers to create analysis contexts from GPR files

with GNATCOLL.Projects;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Libadalang.Analysis;       use Libadalang.Analysis;
with Libadalang.Implementation; use Libadalang.Implementation;

private package Libadalang.GPR_Impl is

   procedure Initialize_Context_From_Project
     (Context          : Internal_Context;
      Tree             : GNATCOLL.Projects.Project_Tree_Access;
      Project          : GNATCOLL.Projects.Project_Type;
      Env              : GNATCOLL.Projects.Project_Environment_Access;
      Is_Project_Owner : Boolean;
      Event_Handler    : Internal_Event_Handler_Access;
      With_Trivia      : Boolean;
      Tab_Stop         : Positive);
   --  Initialize a freshly allocated analysis context from a GPR project.
   --
   --  The unit provider, file reader, config pragmas and default charset are
   --  inferred from the designated project: see
   --  ``Libadalang.Project_Provider.Create_Project_Unit_Provider`` for the
   --  semantics of the ``Tree``, ``Project``, ``Env`` and ``Is_Project_Owner``
   --  arguments.
   --
   --  See ``Libadalang.Implementation.Initialize_Context`` for the semantics
   --  of the other arguments.

   procedure Initialize_Context_From_Project
     (Context       : Internal_Context;
      Tree          : GPR2.Project.Tree.Object;
      Project       : GPR2.Project.View.Object := GPR2.Project.View.Undefined;
      Event_Handler : Internal_Event_Handler_Access;
      With_Trivia   : Boolean;
      Tab_Stop      : Positive);
   --  Likewise, but for GPR2

end Libadalang.GPR_Impl;
