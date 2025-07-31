procedure Test is
   type E is (X, Y, Z);

   package A is
      type Root is tagged record X : Integer; end record;
      function Make return Root is (X => 0);
      function Make2 return Root is (X => 1);
   end A;
   use A;
   package B is
      type Child is new Root with null record;
      overriding function Make return Child is (X => -1);
      overriding function Make2 return Child is (X => -2);
   end B;

   U : Root'Class := B.Child'(X => 3);
   O : E := X;
begin
   U := (case O is when X => Make, when others => Make2);
   pragma Test_Statement;
end Test;
