procedure Test is
   type T is range 1 .. 100;
   type NT is new Test.T'Base;
   type A is tagged null record;
   subtype AA is A'Class;
   type C is array (Integer) of Natural;
   type CC is array (Integer range <>) of Integer;
   type D is new CC (1 .. 12);

   type E (A : Boolean) is record
      case A is
         when True => B : Integer;
         when False => null;
      end case;
   end record;

   O1 : CC := (1, 2, 3, 4);
   O2 : CC (1 .. 10);
   O3 : E (True);
   O4 : E := (A => True, others => <>);
begin
   null;
end Test;
