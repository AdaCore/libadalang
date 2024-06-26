procedure P (N : Natural) is
   type Root is tagged record
      My_Val : Natural := 0;
   end record;

   type Any_Root_Access is access Root'Class;

   Obj : Root;

   Param : Root'Class := Root'Class (Obj);

   X : Any_Root_Access;
begin
   --  All the names in the expression below must be resolved

   X := new P.Root'Class'(Param);
   pragma Test_Statement;
end;
