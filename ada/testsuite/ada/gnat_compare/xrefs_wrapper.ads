--  Set of functions used to adapt LAL's xref results to look more like GNAT's.
--  This can be used as a list of incompatibilities between GNAT and LAL xrefs.

with Libadalang.Analysis; use Libadalang.Analysis;

package Xrefs_Wrapper is

   type Wrapper_Type is
     access function (Decl : Basic_Decl'Class) return Basic_Decl;

   --  All the functions below target a specific construct from LAL's . When
   --  they matches this construct, they try to find the entity that GNAT xref
   --  would yield and return it. Otherwise they return No_Basic_Decl.

   function Subp_Body_Formal (Decl : Basic_Decl'Class) return Basic_Decl;
   --  When a subprogram has both a declaration and a body, GNAT resolves
   --  references to its formals in the body to the formal declarations in the
   --  declaration, while LAL resolves to the formal declaration in the body.
   --
   --  If Decl is formal declaration in a subprogram body, it returns the
   --  corresponding declaration in the subprogram declaration.

   Wrappers : array (Positive range <>) of Wrapper_Type :=
     (1 => Subp_Body_Formal'Access);

end Xrefs_Wrapper;
