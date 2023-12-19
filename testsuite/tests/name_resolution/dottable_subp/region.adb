procedure Region is
   task T;

   task body T is
      type P is tagged null record;
      procedure Not_Prim (Self : P) is null;
      O : P;
   begin
      O.Not_Prim;
      pragma Test_Statement;
   end T;
begin
   declare
      type P2 is tagged null record;
      procedure Not_Prim (Self : P2) is null;
      O : P2;
   begin
      O.Not_Prim;
      pragma Test_Statement;
   end;

   declare
      type P2 is tagged null record;
      procedure Not_Prim (Self : P2'Class) is null;
      O : P2;
   begin
      O.Not_Prim;
      pragma Test_Statement;
   end;
end Region;
