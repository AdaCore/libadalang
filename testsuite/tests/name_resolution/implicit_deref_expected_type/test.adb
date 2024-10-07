procedure Test is
   type Item is record
      Value : Integer;
   end record;

   type Cursor (Ref : access Item) is null record
      with Implicit_Dereference => Ref;

   R : aliased Item := (Value => 42);
   C : Cursor (Ref => R'Access);
   X : Integer := C.Value;
   pragma Test_Statement;
   I : access Item := C.Ref;
   pragma Test_Statement;
begin
   null;
end Test;

