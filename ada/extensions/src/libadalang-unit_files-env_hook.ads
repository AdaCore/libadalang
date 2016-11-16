with Libadalang.Analysis;  use Libadalang.Analysis;
with Libadalang.AST;       use Libadalang.AST;

--  This package exposes primitives to determine for one unit name what are the
--  name of the source files that is supposed to hold it.

package Libadalang.Unit_Files.Env_Hook is

   procedure Env_Hook
     (Unit        : Analysis_Unit;
      Node        : Ada_Node;
      Initial_Env : in out Lexical_Env);
   --  Callback for the lexical environment hook. Fetch the analysis units
   --  that are designated by Node. Node is assumed to be a list of names
   --  coming from a With_Decl node.

end Libadalang.Unit_Files.Env_Hook;
