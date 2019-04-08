package Foo is
   type Arr is array (Integer range 1 .. 3) of Integer;

   type T is tagged record
      Bar : Arr;
   end record;

   function Bar (Self : T) return Integer;
end Foo;
