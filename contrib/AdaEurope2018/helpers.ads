with Libadalang.Analysis;

package Helpers is

   package LAL renames Libadalang.Analysis;

   procedure Iterate_Units
     (Initialize : access procedure (Context : LAL.Analysis_Context) := null;
      Process    : not null access procedure (Unit : LAL.Analysis_Unit);
      Summarize  : access procedure (Context : LAL.Analysis_Context) := null);
   --  Open the "material.gpr" project file in a Libadalang analysis context,
   --  and then:
   --
   --    * If Initialize is passed, call it on the resulting analysis context.
   --
   --    * Then call Process on all analysis units we get for the source files
   --      in this project.
   --
   --    * If Summarize is passed, call it before destroying the analysis
   --      context.

end Helpers;
