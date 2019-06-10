generic
   with function F return Integer;
package Pkg_G is
   function Baz return Integer is (F);
end Pkg_G;
