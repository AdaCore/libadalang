package body Iterators is
   function Consume (I : in Iterator'Class) return Element_Array is
      Element : Element_Type;
      B       : Boolean;
   begin
      B := I'Unrestricted_Access.Next (Element);
      pragma Test_Statement;
      I'Unrestricted_Access.Foo;
      pragma Test_Statement;

      declare
         Result : Element_Array (1 .. 4);
      begin
         return Result;
      end;
   end Consume;
end Iterators;
