procedure Test_Iterable is
   type C is null record;
   type P is null record
     with Iterable =>
       (First => Fist,
        Next => Next,
        Has_Element => Has_Element);
   pragma Test_Block (Expect_Fail => True);

   function First (Self : P) return C is (null record);
   function Next (Self : P; Cursor : C) return C is (null record);
   function Has_Element (Self : P; Cursor : C) return Boolean is (False);
begin
   null;
end Test_Iterable;
