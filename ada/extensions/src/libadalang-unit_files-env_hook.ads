--  This package exposes primitives to determine for one unit name what are the
--  name of the source files that is supposed to hold it.

package Libadalang.Unit_Files.Env_Hook is

   procedure Env_Hook (Unit : Internal_Unit; Node : Bare_Ada_Node);
   --  Callback for the lexical environment hook. Fetch the analysis units
   --  that are designated by Node. Node is assumed to be a list of names
   --  coming from a With_Decl node.

end Libadalang.Unit_Files.Env_Hook;
