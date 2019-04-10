procedure Main is
   type Rec (F : not null access Integer) is null record
   with Implicit_Dereference => F;

   I : aliased Integer := 3;
   X : Rec (I'Access);
begin
   I := X;
   pragma Test_Statement;
   X := I;
   pragma Test_Statement;
end Main;
