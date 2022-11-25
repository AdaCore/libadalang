procedure Test is
   type Some_Interface is interface;
   generic
      type Cursor;
   package Iterator_Interface is
      type Iterator is interface;

      function Get (It : Iterator) return Cursor is abstract;
   end Iterator_Interface;

   generic
      type Elt_Type;
   package Gen is
      type Cursor is access Elt_Type;

      package Int is new Iterator_Interface (Cursor);

      type My_Iterator is abstract
         new Some_Interface
         and Int.Iterator
         with null record;
   end Gen;

   package Inst is new Gen (Integer);

   procedure Foo (X : Inst.My_Iterator'Class) is
      I : access Integer := Inst.Get (X);
      pragma Test_Statement;
   begin
      null;
   end Foo;
begin
   null;
end Test;
