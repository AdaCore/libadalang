--   Test that symbol lookups in some lexical environments do not "leak" in
--   their parents's lexical environment when they should not, as it is the
--   case when resolving qualified names.

package Foo is

   package Bar is
      type T1 is new Integer;
   end;

   type T2 is new Integer;

   pragma Test (Bar.T2);
end Foo;
