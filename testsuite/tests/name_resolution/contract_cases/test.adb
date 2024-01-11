procedure Test is
   procedure Foo (I : Integer)
      with Contract_Cases =>
        ((case I is
            when 1 | 2 | 3 => True,
            when others    => False) => True);
   pragma Test_Block;

   procedure Bar (I : Integer);
   pragma Contract_Cases
     (((case I is
         when 1 | 2 | 3 => True,
         when others    => False) => True));
   pragma Test_Statement;

   procedure Baz (I : Integer)
      with Contract_Cases =>
        (I < 2 => I < 0,
         I > 5 => True,
         others => False);
   pragma Test_Block;

   function To_String (S : String) return String is (S);
   procedure Qux (S : String)
      with Contract_Cases =>
        [True => To_String (S) = [1 .. 10 => 'C']];
   pragma Test_Block;
begin
   null;
end Test;

