--  GNAT can emit degenerate entries for types in (uninstantiated) generics:
--  make sure Libadalang does not crash on them (i.e. ignore them).

generic
   type T is private;
package Gen_Array is
   type Array_Type is array (Integer range <>) of T;
end Gen_Array;
