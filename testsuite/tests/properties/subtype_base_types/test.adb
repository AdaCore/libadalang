procedure Test is
   type A is interface;
   type B is interface;
   type C is new A and B with null record;

   subtype D is C;
begin
   null;
end Test;
