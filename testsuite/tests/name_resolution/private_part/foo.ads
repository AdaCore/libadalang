generic
   type T is private;
   Default_Value : T;
package Foo is
   procedure P;
private
   type Object_Type is tagged record
      Value : T;
   end record;
end Foo;
