--  Set of functions used to adapt LAL's xref results to look more like GNAT's.
--  This can be used as a list of incompatibilities between GNAT and LAL xrefs.

with Libadalang.Analysis; use Libadalang.Analysis;

package Xrefs_Wrapper is

   type Pre_Wrapper_Type is
     access function (Node : Ada_Node'Class) return Basic_Decl;

   type Post_Wrapper_Type is
     access function (Decl : Basic_Decl'Class) return Basic_Decl;

   --  All the functions below target a specific construct from LAL's . When
   --  they matches this construct, they try to find the entity that GNAT xref
   --  would yield and return it. Otherwise they return No_Basic_Decl.

   function Record_Discriminant (Node : Ada_Node'Class) return Basic_Decl;
   --  GNAT resolves the definition of a discriminant to the identifier of the
   --  embedding type.
   --
   --  If Decl is an Identifier under a Discriminant_Spec node, return the
   --  embedding type declaration.

   function Subp_Body_Formal (Decl : Basic_Decl'Class) return Basic_Decl;
   --  When a subprogram has both a declaration and a body, GNAT resolves
   --  references to its formals in the body to the formal declarations in the
   --  declaration, while LAL resolves to the formal declaration in the body.
   --
   --  If Decl is formal declaration in a subprogram body, return the
   --  corresponding declaration in the subprogram declaration.

   function Generic_Package (Decl : Basic_Decl'Class) return Basic_Decl;
   --  GNAT resolves to the identifier of a generic package whereas LAL
   --  resolves to the top-level "generic" declaration.
   --
   --  If Decl is a Generic_Package_Decl, return the underlying
   --  Generic_Package_Internal node.

   function Generic_Subp (Decl : Basic_Decl'Class) return Basic_Decl;
   --  GNAT resolves to the identifier of a generic procedure whereas LAL
   --  resolves to the top-level "generic" declaration.
   --
   --  If Decl is a Generic_Subp_Decl, return the underlying
   --  Generic_Subp_Internal node.

   Pre_Wrappers : array (Positive range <>) of Pre_Wrapper_Type :=
     (1 => Record_Discriminant'Access);
   Post_Wrappers : array (Positive range <>) of Post_Wrapper_Type :=
     (Subp_Body_Formal'Access,
      Generic_Package'Access,
      Generic_Subp'Access);

end Xrefs_Wrapper;
