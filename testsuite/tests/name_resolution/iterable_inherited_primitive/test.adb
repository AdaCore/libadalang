procedure Test is
   package Pkg is
      type T is tagged null record;

      function First (X : T) return Integer is (1);
      function Next (X : T; C : Integer) return Integer is (C + 1);
      function Has_Element (X : T; C : Integer) return Boolean is (True);
   end Pkg;

   package Iter is
      type T is new Pkg.T with null record
         with Iterable => (First       => First,
                           Next        => Next,
                           Has_Element => Has_Element);
      pragma Test_Block;
   end Iter;
begin
   null;
end Test;
