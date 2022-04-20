procedure Main is
   type Data is record
      E : Integer;
   end record;

   type Rec (F : not null access Data) is null record
   with Implicit_Dereference => F;

   I : aliased Data := (E => 2);
   X : Rec (I'Access);
begin
   I := X;
   pragma Test_Statement;
   X := I;
   pragma Test_Statement;
   X := X;
   pragma Test_Statement;
   X := (E => 2);
   pragma Test_Statement;
end Main;
