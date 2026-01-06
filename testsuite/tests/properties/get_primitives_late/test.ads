package Test is
   type T_Ancestor is null record;
   --% ancestor_prims = node.p_get_primitives(False)
   --% len(ancestor_prims) == 2

   type T_Descendant is new T_Ancestor;
   --% descendant_prims = node.p_get_primitives(False)
   --% len(descendant_prims) == 1
   --% descendant_inherited_prims = node.p_get_primitives(True)
   --% len(descendant_inherited_prims) == 0

   -- These operations are primitives of T_Ancestor, but because they are
   -- declared "late", i.e., after the declaration of T_Descendant, these
   -- primitives are not inhereted by the latter. It means, for example, that
   -- the primitive of T_Descendant can't override the primitive of T_Ancestor
   -- since it is not inherited by T_Descendant. Note that this is only
   -- possible for untagged types (this code would be invalid for tagged
   -- types).
   function Compute (Left, Right: T_Ancestor) return Float;
   function Compute (Left : T_Ancestor) return Float;

   function Compute (Left, Right: T_Descendant) return Float;
end Test;
