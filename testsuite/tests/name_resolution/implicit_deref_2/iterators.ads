generic
   type Element_Type is private;
   type Element_Array is array (Positive range <>) of Element_Type;
package Iterators is

   type Iterator is interface;

   function Next
     (I       : in out Iterator;
      Element : out Element_Type) return Boolean is abstract;

   procedure Foo (I : access Iterator) is abstract;

   function Consume (I : in Iterator'Class) return Element_Array;

end Iterators;
