with Vector;

procedure Main is
   package Int_Vector is new Vector (Integer);
   package Float_Vector is new Vector (Float);

   V  : Int_Vector.Vector;
   V2 : Float_Vector.Vector;

   use Int_Vector;
   use Float_Vector;
begin
   Append (V2, 12.0);
   pragma Test_Statement;

   Append (V, 12);
   pragma Test_Statement;
end Main;
