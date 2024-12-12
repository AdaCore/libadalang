package Records is
   type Rec1 is record
      B : Boolean;
      C : Character;
      I : Integer;
   end record;

   type Rec2 is null record;

   type Rec3 is new Rec1;
   pragma Pack (Rec3);
end Records;
