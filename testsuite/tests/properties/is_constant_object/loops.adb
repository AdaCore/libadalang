procedure Loops is
   type Arr is array (1 .. 3) of Integer;
   type R1 is record
      A : Arr;
   end record;

   A : Arr := (1, 2, 3);
   B : constant Arr := (1, 2, 3);

   C : constant R1 := (A => A);
begin
   for I in A'Range loop
      null;
   end loop;
   --% node.find(lal.ForLoopVarDecl).p_is_constant_object

   for I of A loop
      null;
   end loop;
   --% node.find(lal.ForLoopVarDecl).p_is_constant_object

   for I in B'Range loop
      null;
   end loop;
   --% node.find(lal.ForLoopVarDecl).p_is_constant_object

   for I of B loop
      null;
   end loop;
   --% node.find(lal.ForLoopVarDecl).p_is_constant_object

   for I of C.A loop
      null;
   end loop;
   --% node.find(lal.ForLoopVarDecl).p_is_constant_object
end Loops;
