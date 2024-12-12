package Records is
   type Rec1 is record
      B : Boolean;
      C : Character;
      I : Integer;
   end record;

   type Rec2 is null record;
end Records;
