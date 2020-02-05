procedure Test is
   type A is range 1 .. 10;
   subtype B is A range 5 .. 6;
   subtype C is B;
   type D is new A;

   type E is tagged null record;
   type F is new E with null record;
begin
   null;
end Test;
