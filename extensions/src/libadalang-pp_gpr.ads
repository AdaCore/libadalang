--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helper for ``Libadalang.Preprocessing``, implementing preprocessor data
--  extraction from GPR files.

with GNATCOLL.Projects;
with GNATCOLL.Strings; use GNATCOLL.Strings;
with GPR2.Project.Registry.Attribute;
with GPR2.Project.Tree;
with GPR2.Project.View;

with Libadalang.Preprocessing; use Libadalang.Preprocessing;

private package Libadalang.PP_GPR is

   package GPR1 renames GNATCOLL.Projects;

   -------------------------------------------------
   -- Abstraction layer around GPR1 and GPR2 APIs --
   -------------------------------------------------

   type Project_Kind is (GPR1_Kind, GPR2_Kind);

   type Any_Tree (Kind : Project_Kind := Project_Kind'First) is record
      case Kind is
         when GPR1_Kind => GPR1_Value : GPR1.Project_Tree;
         when GPR2_Kind =>
           GPR2_Value : access constant GPR2.Project.Tree.Object;
      end case;
   end record;

   type Any_View (Kind : Project_Kind := Project_Kind'First) is record
      case Kind is
         when GPR1_Kind => GPR1_Value : GPR1.Project_Type;
         when GPR2_Kind => GPR2_Value : GPR2.Project.View.Object;
      end case;
   end record;

   type Any_Attribute (Kind : Project_Kind := Project_Kind'First) is record
      case Kind is
         when GPR1_Kind =>
            GPR1_Value : access constant GPR1.Attribute_Pkg_List;
         when GPR2_Kind => GPR2_Value : GPR2.Q_Attribute_Id;
      end case;
   end record;

   function No_View (Tree : Any_Tree) return Any_View;
   --  Return the "no view" value corresponding to the project kind for
   --  ``Tree``.

   function Root (Self : Any_Tree) return Any_View;
   --  Return the root project for ``Self``

   procedure Iterate
     (Self : Any_View; Process : access procedure (Self : Any_View));
   --  Call ``Process`` on all views accessible from ``Self``

   function Object_Dir (Self : Any_View) return String;
   --  Return the object directory for ``Self``

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
   --  Return whether ``Filename`` is an Ada source file that belongs to the
   --  ``Self`` project.

   package Attributes is
      type Map is array (Project_Kind) of Any_Attribute;

      Default_Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Compiler", "Default_Switches");

      Default_Switches : constant Map :=
        (GPR1_Kind => (Kind       => GPR1_Kind,
                       GPR1_Value => Default_Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value =>
              GPR2.Project.Registry.Attribute.Compiler.Default_Switches));

      Switches_Impl : aliased constant GPR1.Attribute_Pkg_List :=
        GPR1.Build ("Compiler", "Switches");

      Switches : constant Map :=
        (GPR1_Kind => (Kind => GPR1_Kind, GPR1_Value => Switches_Impl'Access),
         GPR2_Kind =>
           (Kind       => GPR2_Kind,
            GPR2_Value => GPR2.Project.Registry.Attribute.Compiler.Switches));

   end Attributes;

   ------------------------
   -- GPR data extractor --
   ------------------------

   procedure Extract_Preprocessor_Data_From_Project
     (Tree           : Any_Tree;
      View           : Any_View;
      Default_Config : out File_Config;
      File_Configs   : out File_Config_Maps.Map)
      with Pre => Tree.Kind = View.Kind;
   --  Create preprocessor data from compiler arguments found in the given GPR
   --  project (``-gnateP`` and ``-gnateD`` arguments).
   --
   --  If a non-null ``View`` is given, look for compiler arguments in it and
   --  the other projects in its closure.  If ``View`` is left to
   --  ``No_View``, try to use the whole project tree.
   --
   --  Note that this function collects all arguments and returns an
   --  approximation from them: it does not replicates exactly gprbuild's
   --  behavior.

end Libadalang.PP_GPR;
