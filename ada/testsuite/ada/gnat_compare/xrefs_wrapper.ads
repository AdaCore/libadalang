--  Set of functions used to adapt LAL's xref results to look more like GNAT's.
--  This can be used as a list of incompatibilities between GNAT and LAL xrefs.

with Libadalang.Analysis; use Libadalang.Analysis;

package Xrefs_Wrapper is

   type Pre_Wrapper_Type is
     access function (Node : Ada_Node) return Defining_Name;

   type Post_Wrapper_Type is
     access function (Decl : Defining_Name) return Defining_Name;

   --  All the functions below target a specific construct from LAL's . When
   --  they matches this construct, they try to find the entity that GNAT xref
   --  would yield and return it. Otherwise they return No_Basic_Decl.

   function Subp_Body_Formal (DN : Defining_Name) return Defining_Name;
   --  When a subprogram has both a declaration and a body, GNAT resolves
   --  references to its formals in the body to the formal declarations in the
   --  declaration, while LAL resolves to the formal declaration in the body.
   --
   --  If Decl is formal declaration in a subprogram body, return the
   --  corresponding declaration in the subprogram declaration.

   function Subp_Body (DN : Defining_Name) return Defining_Name;
   --  When a subprogram has both a declaration and a body, GNAT resolves
   --  references to this subprogram (like in calls) that have visibility
   --  on both to the declaration, while LAL resolves to the body.
   --
   --  If Decl is a subprogram body that has a separate declaration, return the
   --  corresponding declaration.

   function Generic_Package (DN : Defining_Name) return Defining_Name;
   --  GNAT resolves to the identifier of a generic package whereas LAL
   --  resolves to the top-level "generic" declaration.
   --
   --  If Decl is a Generic_Package_Decl, return the underlying
   --  Generic_Package_Internal node.

   function Generic_Subp (DN : Defining_Name) return Defining_Name;
   --  GNAT resolves to the identifier of a generic procedure whereas LAL
   --  resolves to the top-level "generic" declaration.
   --
   --  If Decl is a Generic_Subp_Decl, return the underlying
   --  Generic_Subp_Internal node.

   function Private_Type (DN : Defining_Name) return Defining_Name;
   --  GNAT resolves type references to the first part of a type declaration
   --  (the incomplete one, or the private one) whereas LAL resolves to the
   --  most complete view.
   --
   --  If Decl is a Base_Type_Decl, return the result of P_Previous_Part
   --  (Go_To_Incomplete => True).

   Post_Wrappers : array (Positive range <>) of Post_Wrapper_Type :=
     (Subp_Body_Formal'Access,
      Subp_Body'Access,
      Generic_Package'Access,
      Generic_Subp'Access,
      Private_Type'Access);

end Xrefs_Wrapper;
