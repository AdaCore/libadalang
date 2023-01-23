--  Trigger an exception in ``AdaNode.p_resolve_own_names``, which implements
--  its own memoization mechanism.

package Foo is
   Object : Some_Type.;
   --% node.p_resolve_names
   --% node.p_resolve_names
end Foo;
