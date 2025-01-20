--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

with Libadalang.Analysis;
with Libadalang.Common; use Libadalang.Common;

package Libadalang.Unit_Files is

   use Support.Text;

   package LAL renames Libadalang.Analysis;

   function Default_Provider return LAL.Unit_Provider_Reference;
   --  Default implementation for the Unit_Provider mechanism. It assumes that
   --  each compilation unit gets its own source file in the current directory,
   --  named according to the GNAT convention: See
   --  <http://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn
   --  /the_gnat_compilation_model.html#file-naming-rules> for more details. It
   --  also assumes these sources use GNAT's default native runtime.

   function Unit_String_Name (Name : Text_Type) return String;
   --  Return Name after lowering its case and encoding it appropriately for
   --  the file system.

   function File_From_Unit
     (Name : Text_Type; Kind : Analysis_Unit_Kind) return String;
   --  Convert an unit name and unit kind into the default filename

   subtype Root_Nodes is Ada_Node_Kind_Type with Static_Predicate =>
      Root_Nodes in Ada_Compilation_Unit | Ada_Compilation_Unit_List
                  | Ada_Pragma_Node_List;
   --  Possible nodes at the root of analysis units. Since the grammar allows
   --  for empty lists of compilation units, it is safe to assume that all
   --  analysis units have non-null roots, even when there are parsing errors.

end Libadalang.Unit_Files;
