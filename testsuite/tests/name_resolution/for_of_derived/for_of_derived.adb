with Vectors;

procedure For_Of_Derived is
   package My_Vecs is new Vectors (Natural, Integer);

   type The_Vec is new My_Vecs.Vector with null record;

   V : The_Vec;
begin
   for El of V loop
      null;
   end loop;
end For_Of_Derived;
pragma Test_Block;
