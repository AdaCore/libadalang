--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helpers to do projects processing on project files regardless of whether
--  they were loaded from GPR1 or GPR2 libraries.

with Ada.Containers.Vectors;

with GNATCOLL.File_Paths; use GNATCOLL.File_Paths;
with GNATCOLL.Projects;
with GNATCOLL.Strings;    use GNATCOLL.Strings;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;
with GPR2.Project.View.Vector;

private package Libadalang.GPR_Utils is

   package GPR1 renames GNATCOLL.Projects;

   -------------------------------------------------
   -- Abstraction layer around GPR1 and GPR2 APIs --
   -------------------------------------------------

   type Project_Kind is (GPR1_Kind, GPR2_Kind);

   type Any_Tree (Kind : Project_Kind := Project_Kind'First) is record
      case Kind is
         when GPR1_Kind => GPR1_Value : GPR1.Project_Tree_Access;
         when GPR2_Kind => GPR2_Value : GPR2.Project.Tree.Object;
      end case;
   end record;

   type Any_View (Kind : Project_Kind := Project_Kind'First) is record
      case Kind is
         when GPR1_Kind => GPR1_Value : GPR1.Project_Type;
         when GPR2_Kind => GPR2_Value : GPR2.Project.View.Object;
      end case;
   end record;

   package View_Vectors is new Ada.Containers.Vectors (Positive, Any_View);

   type Attribute_Kind is (GPR1_String, GPR1_List, GPR2_Kind);

   type Any_Attribute (Kind : Attribute_Kind := Attribute_Kind'First) is record
      case Kind is
         when GPR1_String =>
            GPR1_String_Value : access constant GPR1.Attribute_Pkg_String;
         when GPR1_List =>
            GPR1_List_Value : access constant GPR1.Attribute_Pkg_List;
         when GPR2_Kind =>
            GPR2_Value : GPR2.Q_Attribute_Id;
      end case;
   end record;

   function No_View (Tree : Any_Tree) return Any_View;
   --  Return the "no view" value corresponding to the project kind for
   --  ``Tree``.

   function Root (Self : Any_Tree) return Any_View;
   --  Return the root project for ``Self``

   function Name (Self : Any_View) return String;
   --  Return the name for the ``Self`` project

   function Project_File (Self : Any_View) return String;
   --  Return the project file name for ``Self``

   function Dir_Name (Self : Any_View) return String;
   --  Return the absolute name for the project directory for ``Self``

   procedure Iterate
     (Self : Any_View; Process : access procedure (Self : Any_View));
   --  Call ``Process`` on all views accessible from ``Self``

   function Is_Aggregate_Project (Self : Any_View) return Boolean;
   --  Return whether ``Self`` is an aggregate project

   function Aggregated_Projects (Self : Any_View) return View_Vectors.Vector;
   --  Assuming that ``Self`` is an aggregate project, return the list of
   --  non-aggregate roots in its closure.

   function Is_Extended (Self : Any_View) return Boolean;
   --  Return whether ``Self`` is extended by another project

   function Source_Dirs_Path
     (Tree : Any_Tree; View : Any_View) return Any_Path;
   --  Return a search path that gives access to all source files in the
   --  ``View`` project subtree if given, or the whole project otherwise.

   function Object_Dir (Self : Any_View) return String;
   --  Return the object directory for ``Self``

   function Value (Self : Any_View; Attribute : Any_Attribute) return String;
   --  Return the string assoicated to the given string ``Attribute`` in the
   --  ``Self`` project. Return an empty string if the attribute is not
   --  defined.

   function Indexes
     (Self : Any_View; Attribute : Any_Attribute) return XString_Array;
   --  Return the list of indexes used in attributes definition for
   --  ``Attribute`` in the ``Self`` project.

   function Values
     (Self      : Any_View;
      Attribute : Any_Attribute;
      Index     : String) return XString_Array;
   --  Return the list of strings associated to the given list ``Attribute`` at
   --  the given ``Index`` in the ``Self`` project. Return an empty list if
   --  this attribute is not defined for this index.

   --  Define constant for references to attributes. ``Default_Switches`` and
   --  ``Switches`` in the ``Compiler`` package.

   function Is_Ada_Source
     (Tree : Any_Tree; View : Any_View; Filename : String) return Boolean;
   --  Return whether ``Filename`` (a base name) is an Ada source file that
   --  belongs to the ``View`` project.

   type Any_Unit_Part is (Unit_Spec, Unit_Body);

   function Filename_For_Unit
     (View : Any_View; Unit_Name : String; Part : Any_Unit_Part) return String;
   --  Return the basename for the source file that contains the given unit

   procedure Iterate_Ada_Units
     (Tree      : Any_Tree;
      View      : Any_View;
      Process   : access procedure (Unit_Name : String;
                                    Unit_Part : Any_Unit_Part;
                                    Filename  : String);
      Recursive : Boolean := True);
   --  Iterate over all Ada units/source files in the ``View`` project
   --  hierarchy (if ``Recursive`` is true) or in ``View`` alone (otherwise).

   procedure Iterate_Ada_Compiler_Switches
     (Tree    : Any_Tree;
      View    : Any_View;
      Process : access procedure (View : Any_View; Switch : XString));
   --  Iterate over all Ada source files in the ``View`` project hierarchy (or
   --  ``Tree``'s root project if ``View`` is ``No_View (Tree)``), and call
   --  ``Process`` on all Ada compiler switches that are found, passing to it
   --  the view in which the switch was found.

   package Attributes is
      type Map is array (Project_Kind) of Any_Attribute;

      Default_Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Compiler", "Default_Switches");

      Default_Switches : constant Map :=
        (GPR1_Kind => (Kind            => GPR1_List,
                       GPR1_List_Value => Default_Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value =>
              GPR2.Project.Registry.Attribute.Compiler.Default_Switches));

      Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Compiler", "Switches");

      Switches : constant Map :=
        (GPR1_Kind => (Kind            => GPR1_List,
                       GPR1_List_Value => Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value => GPR2.Project.Registry.Attribute.Compiler.Switches));

      Global_Pragmas_Impl : aliased constant GPR1.Attribute_Pkg_String :=
        GPR1.Global_Pragmas_Attribute;

      Global_Pragmas_Attribute : constant Map :=
        (GPR1_Kind => (Kind              => GPR1_String,
                       GPR1_String_Value => Global_Pragmas_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value => GPR2.Project.Registry.Attribute
                          .Builder.Global_Configuration_Pragmas));

      Global_Comp_Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Builder", "Global_Compilation_Switches");

      Global_Compilation_Switches : constant Map :=
        (GPR1_Kind => (Kind            => GPR1_List,
                       GPR1_List_Value => Global_Comp_Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value =>
              GPR2
              .Project
              .Registry
              .Attribute
              .Builder
              .Global_Compilation_Switches));

      Builder_Dflt_Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Builder", "Default_Switches");

      Builder_Default_Switches : constant Map :=
        (GPR1_Kind =>
           (Kind            => GPR1_List,
            GPR1_List_Value => Builder_Dflt_Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value =>
              GPR2.Project.Registry.Attribute.Builder.Default_Switches));

      Builder_Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Builder", "Switches");

      Builder_Switches : constant Map :=
        (GPR1_Kind => (Kind            => GPR1_List,
                       GPR1_List_Value => Builder_Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value => GPR2.Project.Registry.Attribute.Builder.Switches));

      Local_Pragmas_Impl : aliased constant GPR1.Attribute_Pkg_String :=
        GPR1.Local_Pragmas_Attribute;

      Local_Pragmas_Attribute : constant Map :=
        (GPR1_Kind => (Kind              => GPR1_String,
                       GPR1_String_Value => Local_Pragmas_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value => GPR2.Project.Registry.Attribute
                          .Compiler.Local_Configuration_Pragmas));

   end Attributes;

   ------------------
   -- GPR2 helpers --
   ------------------

   function Lookup
     (Self         : GPR2.Project.Tree.Object;
      Project_Name : String) return GPR2.Project.View.Object;
   --  Return the view that ``Project_Name`` designates: it may be either a
   --  project name (case insensitive Ada identifier) or a (possibly relative)
   --  project file name.
   --
   --  If no project match or if multiple projects match, raise a
   --  ``GPR2.Project_Error`` exception.

   function Closure
     (Self : GPR2.Project.View.Object) return GPR2.Project.View.Vector.Object;
   --  Return the list of all views reachable from ``Self``, self included

end Libadalang.GPR_Utils;
