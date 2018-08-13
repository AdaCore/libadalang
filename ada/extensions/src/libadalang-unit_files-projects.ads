with GNATCOLL.Projects;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

--  This package provides an Unit_Provider implemetation that relies on a
--  project file.

package Libadalang.Unit_Files.Projects is

   package LP renames Libadalang.Analysis;

   type Project_Unit_Provider is new LP.Unit_Provider_Interface with private;
   --  Unit_Provider implementation that relies on a project file

   package Prj renames GNATCOLL.Projects;

   function Create_Project_Unit_Provider
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean)
      return Project_Unit_Provider;
   --  Create an unit provider using Project. If Is_Project_Owner is true,
   --  the result owns Project, thus the caller must not deallocate it itself.
   --  Otherwise, the project pointed by Project must outlive the returned unit
   --  file provider.

   overriding function Get_Unit_Filename
     (Provider : Project_Unit_Provider;
      Name     : Text_Type;
      Kind     : Unit_Kind) return String;

   overriding function Get_Unit
     (Provider    : Project_Unit_Provider;
      Context     : LP.Analysis_Context'Class;
      Name        : Text_Type;
      Kind        : Unit_Kind;
      Charset     : String := "";
      Reparse     : Boolean := False) return LP.Analysis_Unit'Class;

   function Convert (Kind : Unit_Kind) return GNATCOLL.Projects.Unit_Parts is
     (case Kind is
      when Unit_Specification => GNATCOLL.Projects.Unit_Spec,
      when Unit_Body          => GNATCOLL.Projects.Unit_Body);

private

   type Project_Unit_Provider is new LP.Unit_Provider_Interface with record
      Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean;
   end record;

   overriding procedure Release (Provider : in out Project_Unit_Provider);

   function Create_Project_Unit_Provider
     (Project          : Prj.Project_Tree_Access;
      Env              : Prj.Project_Environment_Access;
      Is_Project_Owner : Boolean) return Project_Unit_Provider
   is ((Project          => Project,
        Env              => Env,
        Is_Project_Owner => Is_Project_Owner));

end Libadalang.Unit_Files.Projects;
