limited with Foo;

package Bar is

   type Bar_Type is record
      F : access Foo_Type;
   end record;

end Bar;
