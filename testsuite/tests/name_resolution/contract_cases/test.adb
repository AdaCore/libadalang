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
begin
   null;
end Test;

