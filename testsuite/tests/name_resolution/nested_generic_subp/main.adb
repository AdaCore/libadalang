with Vector;

procedure Main is
   package My_Vector is new Vector (Integer);

   use My_Vector;

   function "<" (L, R : Integer) return Boolean is (L > R);
   procedure Int_Vec_Sort is new My_Vector.Sort ("<");

   V : My_Vector.Vector := Create;
begin
   Int_Vec_Sort (V);
   pragma Test_Statement;
end Main;
