with Vector_Ops;

procedure Main is
   package My_Vector_Ops is new Vector_Ops (Integer);

   function Is_Even (I : Integer) return Boolean is
     (I mod 2 = 0);

   function Filter_Even is new My_Vector_Ops.Filter (Is_Even);

   V : My_Vector_Ops.Internal_Vector.Vector;
begin
   -- pragma Test (My_Vector_Ops.Internal_Vector.Vector);
   V := Filter_Even (V);
   pragma Test_Statement;
end Main;
