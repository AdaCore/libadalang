procedure Test is
   TYPE DIS (A : BOOLEAN) IS RECORD
      CASE A IS
         WHEN TRUE =>
            B : BOOLEAN;
            C : INTEGER;
         WHEN FALSE =>
            D : INTEGER;
      END CASE;
   END RECORD;

   D : DIS := (C => 3, OTHERS => TRUE);
   pragma Test_Statement;
begin
   null;
end Test;
