package Test is
   type T_Ancestor is null record;
   --% ancestor_prims = node.p_get_primitives()

   type T_Descendant is new T_Ancestor;
   --% descendant_prims = node.p_get_primitives()
   --% descendant_inherited_prims = node.p_get_primitives(True)
   --% len(descendant_inherited_prims) == 0

   -- This operation is a primitive of T_Ancestor, but because it is declared
   -- "late", i.e., after the declaration of T_Descendant, that primitive is not
   -- inhereted by the latter. It means, for example, that the primitive of
   -- T_Descendant can't override the primitive of T_Ancestor since it is not
   -- inherited by T_Descendant. Note that this is only possible for untagged
   -- types (this code would be invalid for tagged types).
   function Compute (Left, Right: T_Ancestor) return Float;

   function Compute (Left, Right: T_Descendant) return Float;
end Test;
