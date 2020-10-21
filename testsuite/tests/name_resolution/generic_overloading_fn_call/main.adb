with Vector;

procedure Main is
   package Int_Vector is new Vector (Integer);
   package Int_Vector_2 is new Vector (Integer);

   V, V2  : Int_Vector.Vector;
   B      : Integer;
begin
   Int_Vector.Append (V, B);
   pragma Test_Statement;

   B := Int_Vector.Append (V, 12);
   pragma Test_Statement;
end Main;
