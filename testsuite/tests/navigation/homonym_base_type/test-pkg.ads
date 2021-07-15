with Test.Impl; use Test.Impl;

package Test.Pkg is
private
   type T is new Test.Impl.T with null record;
   --% node.p_previous_part()
   --% node.p_base_type()
end Test.Pkg;
