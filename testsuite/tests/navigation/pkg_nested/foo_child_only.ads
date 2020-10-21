--  This tests that directly navigating to the body of a nested package (here:
--  Bar) will automatically load the body unit. If that's not the case,
--  navigation will not be able to find the body of the nested package.

package Foo_Child_Only
   with Disable_Navigation
is

   I : Integer;

   package Bar is
      procedure Baz;
   end Bar;

   S : String := "hello!";

end Foo_Child_Only;
