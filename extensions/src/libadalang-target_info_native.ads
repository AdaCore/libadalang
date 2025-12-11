--
--  Copyright (C) 2022-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Target_Info; use Libadalang.Target_Info;

private package Libadalang.Target_Info_Native is

  procedure Get_Builtin_Target_Info
     (Target_Name     : String;
      Target_Info     : out Target_Information;
      Has_Target_Info : out Boolean);
   --  The default runtimes for native targets lack the "ada_target_properties"
   --  files. As a workaround, this function provides correct values for them
   --  here (setting ``Has_Target_Info`` to ``True``), or sets ``Target_Info``
   --  to ``Placeholder_Target_Info`` and ``Has_Target_Info`` to ``False`` for
   --  unhandled targets.

end Libadalang.Target_Info_Native;
