package Test_Pre_Class is
   type T is abstract tagged;

   function Foo
     (Self : T) return Boolean is abstract
   with Pre => Self.Why;
   pragma Test_Block;

   type T is abstract tagged null record;
   function Why (Self : T) return Boolean is (True);

end Test_Pre_Class;
