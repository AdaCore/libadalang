procedure Test is
     TYPE INT IS NEW INTEGER RANGE -100 .. 100;

     PACKAGE PACK IS
          FUNCTION "+" (RIGHT : INT) RETURN INTEGER;
     END PACK;
     use PACK;

     FUNCTION "+" (RIGHT : INT) RETURN INTEGER IS
     BEGIN
          RETURN INTEGER'(1) + INTEGER(RIGHT);
     END "+";
   --% node.p_canonical_part()

begin
   null;
end Test;
