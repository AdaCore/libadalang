------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2020, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--  Extension to store the code for external properties

package Libadalang.Implementation.Extensions is

   --------------
   -- Ada_Node --
   --------------

   function Ada_Node_P_Get_Unit
     (Node           : Bare_Ada_Node;
      Name           : Symbol_Type_Array_Access;
      Kind           : Analysis_Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit;

   function Ada_Node_P_Standard_Unit
     (Node : Bare_Ada_Node) return Internal_Unit;

   function Ada_Node_P_Filter_Is_Imported_By
     (Node       : Bare_Ada_Node;
      Units      : Internal_Unit_Array_Access;
      Transitive : Boolean) return Internal_Unit_Array_Access;

   -------------
   -- Base_Id --
   -------------

   function Base_Id_Short_Image (Node : Bare_Base_Id) return Text_Type;
   --  Custom version of Short_Image for identifiers, so that the identifier
   --  text is part of the image.

   ----------------
   -- Basic_Decl --
   ----------------

   function Basic_Decl_Short_Image (Node : Bare_Basic_Decl) return Text_Type;
   --  Custom version of Short_Image for basic declarations. Include
   --  the names of the entities it declares.

   function Basic_Decl_P_Doc
     (Node : Bare_Basic_Decl) return Character_Type_Array_Access;

   function Basic_Decl_P_Doc_Annotations
     (Node : Bare_Basic_Decl) return Internal_Doc_Annotation_Array_Access;

   ------------------
   -- Char_Literal --
   ------------------

   function Char_Literal_P_Denoted_Value
     (Node : Bare_Char_Literal) return Character_Type;

   ----------------------
   -- Compilation_Unit --
   ----------------------

   function Compilation_Unit_P_Get_Empty_Env
     (Node : Bare_Compilation_Unit) return Lexical_Env;

   ----------
   -- Expr --
   ----------

   function Expr_P_Eval_As_Int_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Big_Integer_Type;

   function Expr_P_Eval_As_String_In_Env
     (Node   : Bare_Expr;
      Env    : Internal_Substitution_Array_Access;
      E_Info : Internal_Entity_Info) return Character_Type_Array_Access;

   ---------------------------
   -- Generic_Instantiation --
   ---------------------------

   function Generic_Instantiation_P_Instantiation_Env
     (Node : Bare_Generic_Instantiation) return Lexical_Env;

   -----------------
   -- Int_Literal --
   -----------------

   function Int_Literal_P_Denoted_Value
     (Node : Bare_Int_Literal) return Big_Integer_Type;

   ----------
   -- Name --
   ----------

   function Name_P_Internal_Referenced_Unit
     (Node           : Bare_Name;
      Kind           : Analysis_Unit_Kind;
      Load_If_Needed : Boolean) return Internal_Unit;

   --------------------
   -- String_Literal --
   --------------------

   function String_Literal_P_Denoted_Value
     (Node : Bare_String_Literal) return Character_Type_Array_Access;

   ---------------
   -- Type_Decl --
   ---------------

   function Type_Decl_P_Primitives (Node : Bare_Type_Decl) return Lexical_Env;

end Libadalang.Implementation.Extensions;
