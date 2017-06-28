with Vector;

procedure Main is
   package My_Vector is new Vector (Integer);
   package My_Vector_2 is new Vector (Float);

   use My_Vector;
   use My_Vector_2;

begin
   declare
      V : My_Vector.Vector'Class := Create;
   begin
      V.Append (12);
   end;
   pragma Test_Block;
end Main;
