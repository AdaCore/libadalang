--
--  Copyright (C) 2014-2022, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Extension to store the code for external properties

package Libadalang.Implementation.Extensions is

   --------------
   -- Ada_Node --
   --------------

   function Ada_Node_P_Get_Unit
     (Node               : Bare_Ada_Node;
      Name               : Symbol_Type_Array_Access;
      Kind               : Analysis_Unit_Kind;
      Load_If_Needed     : Boolean;
      Not_Found_Is_Error : Boolean;
      Process_Parents    : Boolean := True) return Internal_Unit;

   function Ada_Node_P_Standard_Unit
     (Node : Bare_Ada_Node) return Internal_Unit;

   function Ada_Node_P_Filter_Is_Imported_By
     (Node       : Bare_Ada_Node;
      Units      : Internal_Unit_Array_Access;
      Transitive : Boolean) return Internal_Unit_Array_Access;

   function Ada_Node_P_Resolve_Own_Names
     (Node   : Bare_Ada_Node;
      Env    : Lexical_Env;
      Origin : Bare_Ada_Node;
      E_Info : Internal_Entity_Info := No_Entity_Info) return Boolean;

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

   function Basic_Decl_P_Doc (Node : Bare_Basic_Decl) return String_Type;

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

   function Compilation_Unit_P_External_Config_Pragmas
     (Node : Bare_Compilation_Unit) return Bare_Pragma_Node_Array_Access;

   -------------------
   -- Defining_Name --
   -------------------

   function Defining_Name_Short_Image
     (Node : Bare_Defining_Name) return Text_Type;

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
      E_Info : Internal_Entity_Info) return String_Type;

   -----------------
   -- Int_Literal --
   -----------------

   function Int_Literal_P_Denoted_Value
     (Node : Bare_Int_Literal) return Big_Integer_Type;

   --------------------
   -- String_Literal --
   --------------------

   function String_Literal_P_Denoted_Value
     (Node : Bare_String_Literal) return String_Type;

   ----------
   -- Expr --
   ----------

   function Expr_P_Type_Var (Node : Bare_Expr) return Logic_Var;

   function Expr_P_Expected_Type_Var (Node : Bare_Expr) return Logic_Var;

   ---------------------
   -- Single_Tok_Node --
   ---------------------

   function Single_Tok_Node_P_Ref_Var
     (Node : Bare_Single_Tok_Node) return Logic_Var;

   function Single_Tok_Node_P_Subp_Spec_Var
     (Node : Bare_Single_Tok_Node) return Logic_Var;

end Libadalang.Implementation.Extensions;
