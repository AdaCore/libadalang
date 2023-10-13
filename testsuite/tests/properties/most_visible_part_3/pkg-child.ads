--  Test that ``p_most_visible_part`` works as expected inside the public part
--  of child packages: since we don't have view on the private part of the
--  parent package at this point, the property must return the partial (public)
--  view of the designated type.
package Pkg.Child is
   subtype U is T;
   --% full_view = node.p_get_type()
   --% view_from_here = full_view.p_most_visible_part(node)
end Pkg.Child;
