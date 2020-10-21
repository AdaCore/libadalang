procedure Test is
   type T is range 1 .. 100;
   type NT is new Test.T'Base;
   type A is tagged null record;
   subtype AA is A'Class;
   type C is array (Integer) of Natural;
   type CC is array (Integer range <>) of Integer;
   type D is new CC (1 .. 12);
begin
   null;
end Test;
