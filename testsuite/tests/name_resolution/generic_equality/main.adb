with Vector;

procedure Main is
   package Int_Vector is new Vector (Integer);
   package Int_Vector_2 is new Vector (Integer);

   V, V2  : Int_Vector.Vector;
   V3, V4 : Int_Vector_2.Vector;
begin
   V := V2;
   pragma Test_Statement;

   V3 := V4;
   pragma Test_Statement;

   V := V4;
   pragma Test_Statement (Expect_Fail => True);
end Main;
