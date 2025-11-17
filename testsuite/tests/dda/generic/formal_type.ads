--  GNAT can emit degenerate entries for (uninstantiated) generic formal types:
--  make sure Libadalang does not crash on them (i.e. ignore them).

generic
   type T is (<>);
package Formal_Type is
end Formal_Type;
