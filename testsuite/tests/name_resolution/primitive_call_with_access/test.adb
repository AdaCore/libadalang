procedure Test is
   package P is
      type T is tagged null record;

      procedure Proc (X : access T) is null;
   end P;

   package Q is
      type U is new P.T with null record;
   end Q;

   Obj : aliased Q.U;
begin
   Q.Proc (Obj'Access);
   pragma Test_Statement;

   Obj'Access.Proc;
   pragma Test_Statement;
end Test;
